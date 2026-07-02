namespace Nu.Mcp

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open System.Globalization
open System.IO
open System.Numerics
open System.Reflection
open System.Text.Json
open System.Text.Json.Nodes
open Prime
open Nu

module Program =

    type McpPlugin () =
        inherit NuPlugin ()

    type RuntimeEvent =
        { SubscriptionId : string
          Address : string
          Data : string
          Trace : string
          Publisher : string }

    let private startupDir = Directory.GetCurrentDirectory ()

    let mutable private worldOpt : World option = None
    let mutable private gameSessionActive = false
    let mutable private gameHostMode = false
    let mutable private gameHostStopRequested = false
    let mutable private sdlDepsOpt : IDisposable option = None
    let mutable private gameHostProcessOpt : Process option = None
    let mutable private gameHostRequestId = 0
    let private gameHostErrors = ConcurrentQueue<string> ()
    let private eventQueue = ConcurrentQueue<RuntimeEvent> ()
    let private subscriptions = Dictionary<string, World -> unit> StringComparer.Ordinal

    let private jsonOptions = JsonSerializerOptions (WriteIndented = false)

    let private jstr (str : string) = JsonValue.Create str :> JsonNode
    let private jbool (value : bool) = JsonValue.Create value :> JsonNode
    let private jint (value : int) = JsonValue.Create value :> JsonNode
    let private jint64 (value : int64) = JsonValue.Create value :> JsonNode
    let private jnode (node : JsonNode) = node

    let private tryGetNode (name : string) (obj : JsonObject) =
        let mutable node = Unchecked.defaultof<JsonNode>
        if obj.TryGetPropertyValue (name, &node) then Some node else None


    let private textNode (text : string) =
        JsonObject
            ([ KeyValuePair<string, JsonNode> ("type", JsonValue.Create "text")
               KeyValuePair<string, JsonNode> ("text", JsonValue.Create text) ])

    let private toolResult (payload : JsonNode) =
        JsonObject
            ([ KeyValuePair<string, JsonNode> ("content", JsonArray (textNode (payload.ToJsonString jsonOptions)))
               KeyValuePair<string, JsonNode> ("isError", JsonValue.Create false) ])

    let private errorResult (message : string) =
        JsonObject
            ([ KeyValuePair<string, JsonNode> ("content", JsonArray (textNode message))
               KeyValuePair<string, JsonNode> ("isError", JsonValue.Create true) ])

    let private response (id : JsonNode) (result : JsonNode) =
        let obj = JsonObject ()
        obj["jsonrpc"] <- JsonValue.Create "2.0"
        if not (isNull id) then obj["id"] <- id.DeepClone ()
        obj["result"] <- result
        obj.ToJsonString jsonOptions

    let private errorResponse (id : JsonNode) (code : int) (message : string) =
        let err = JsonObject ()
        err["code"] <- JsonValue.Create code
        err["message"] <- JsonValue.Create message
        let obj = JsonObject ()
        obj["jsonrpc"] <- JsonValue.Create "2.0"
        if not (isNull id) then obj["id"] <- id.DeepClone ()
        obj["error"] <- err
        obj.ToJsonString jsonOptions

    let private requiredString (args : JsonObject) (name : string) =
        match tryGetNode name args with
        | Some (:? JsonValue as value) ->
            match value.TryGetValue<string> () with
            | true, str when not (String.IsNullOrWhiteSpace str) -> str
            | _ -> invalidArg name ("Expected non-empty string argument '" + name + "'.")
        | _ -> invalidArg name ("Missing string argument '" + name + "'.")

    let private optionalString (args : JsonObject) (name : string) (defaultValue : string) =
        match tryGetNode name args with
        | Some (:? JsonValue as value) ->
            match value.TryGetValue<string> () with
            | true, str -> str
            | _ -> defaultValue
        | _ -> defaultValue

    let private optionalInt (args : JsonObject) (name : string) (defaultValue : int) =
        match tryGetNode name args with
        | Some (:? JsonValue as value) ->
            match value.TryGetValue<int> () with
            | true, i -> i
            | _ -> defaultValue
        | _ -> defaultValue

    let private optionalBool (args : JsonObject) (name : string) (defaultValue : bool) =
        match tryGetNode name args with
        | Some (:? JsonValue as value) ->
            match value.TryGetValue<bool> () with
            | true, b -> b
            | _ -> defaultValue
        | _ -> defaultValue

    let private world () =
        match worldOpt with
        | Some world -> world
        | None -> invalidOp "No Nu MCP runtime session is active. Call session_start_stub first."

    let private parseSimulant (addressStr : string) : Simulant =
        let names = (stoa<obj> addressStr).Names
        match names.Length with
        | 1 when names[0] = Constants.Engine.GameName -> Nu.Game.Handle :> Simulant
        | 2 -> new Screen (addressStr) :> Simulant
        | 3 -> new Group (addressStr) :> Simulant
        | n when n >= 4 -> new Entity (addressStr) :> Simulant
        | _ -> invalidArg (nameof addressStr) ("Invalid simulant address '" + addressStr + "'.")

    let private exists (simulant : Simulant) (world : World) =
        match simulant with
        | :? Game -> true
        | :? Screen as screen -> screen.GetExists world
        | :? Group as group -> group.GetExists world
        | :? Entity as entity -> entity.GetExists world
        | _ -> false

    let private tryGetProperty (propertyName : string) (simulant : Simulant) (world : World) =
        match simulant with
        | :? Game as game -> game.TryGetProperty propertyName world
        | :? Screen as screen -> screen.TryGetProperty propertyName world
        | :? Group as group -> group.TryGetProperty propertyName world
        | :? Entity as entity -> entity.TryGetProperty propertyName world
        | _ -> None

    let private setPropertyExisting (propertyName : string) (property : Property) (simulant : Simulant) (world : World) : bool =
        match simulant with
        | :? Game as game -> game.SetProperty propertyName property world; true
        | :? Screen as screen -> screen.SetProperty propertyName property world; true
        | :? Group as group -> group.SetProperty propertyName property world; true
        | :? Entity as entity -> entity.SetProperty propertyName property world; true
        | _ -> false

    let private signal (signalObj : obj) (simulant : Simulant) (world : World) =
        match simulant with
        | :? Game as game -> (game.GetDispatcher world).Signal (signalObj, game, world)
        | :? Screen as screen -> (screen.GetDispatcher world).Signal (signalObj, screen, world)
        | :? Group as group -> (group.GetDispatcher world).Signal (signalObj, group, world)
        | :? Entity as entity -> (entity.GetDispatcher world).Signal (signalObj, entity, world)
        | _ -> invalidOp "Unknown simulant type." : unit

    let private symbolFromText text =
        scvalue<Symbol> text

    let private valueText value =
        try valueToSymbol value |> scstring
        with _ -> if isNull value then "null" else string value

    let private propertyJson (propertyName : string) (property : Property) =
        let obj = JsonObject ()
        obj["name"] <- jstr propertyName
        obj["type"] <- jstr property.PropertyType.FullName
        obj["value"] <- jstr (valueText property.PropertyValue)
        obj

    let private convertStringToType (targetType : Type) (text : string) =
        if targetType = typeof<string> then text :> obj
        elif targetType = typeof<bool> then Boolean.Parse text :> obj
        elif targetType = typeof<int> then Int32.Parse (text, CultureInfo.InvariantCulture) :> obj
        elif targetType = typeof<int64> then Int64.Parse (text, CultureInfo.InvariantCulture) :> obj
        elif targetType = typeof<single> then Single.Parse (text, CultureInfo.InvariantCulture) :> obj
        elif targetType = typeof<double> then Double.Parse (text, CultureInfo.InvariantCulture) :> obj
        elif targetType = typeof<Vector2> then scvalue<Vector2> text :> obj
        elif targetType = typeof<Vector3> then scvalue<Vector3> text :> obj
        elif targetType = typeof<Vector4> then scvalue<Vector4> text :> obj
        elif targetType = typeof<Quaternion> then scvalue<Quaternion> text :> obj
        else
            let converter = TypeDescriptor.GetConverter targetType
            if converter.CanConvertFrom typeof<string> then converter.ConvertFromInvariantString text
            else invalidOp ("Property type '" + targetType.FullName + "' is not string-convertible by this MCP façade.")

    let private cleanupSession () =
        match worldOpt with
        | Some world ->
            for unsubscribe in subscriptions.Values do
                try unsubscribe world with _ -> ()
            subscriptions.Clear ()
            let mutable ignored = Unchecked.defaultof<RuntimeEvent>
            while eventQueue.TryDequeue &ignored do ()
            try World.cleanUp world with _ -> ()
            match sdlDepsOpt with
            | Some sdlDeps -> try sdlDeps.Dispose () with _ -> (); sdlDepsOpt <- None
            | None -> ()
            worldOpt <- None
            gameSessionActive <- false
        | None ->
            match sdlDepsOpt with
            | Some sdlDeps -> try sdlDeps.Dispose () with _ -> (); sdlDepsOpt <- None
            | None -> ()
            gameSessionActive <- false

    let private startStubSession () =
        cleanupSession ()
        Log.setLogSynchronously false
        Nu.init ()
        let worldConfig = { WorldConfig.defaultConfig with Imperative = true; Advancing = false }
        let world = World.makeStub (fun () -> None) worldConfig (McpPlugin ())
        worldOpt <- Some world
        gameSessionActive <- false
        JsonObject ([ KeyValuePair<string, JsonNode> ("started", JsonValue.Create true)
                      KeyValuePair<string, JsonNode> ("mode", JsonValue.Create "stub")
                      KeyValuePair<string, JsonNode> ("game", JsonValue.Create (string Nu.Game.Handle)) ])

    let private loadPlugin (assemblyPath : string) (pluginTypeName : string) =
        let assemblyPath = Path.GetFullPath assemblyPath
        if not (File.Exists assemblyPath) then invalidOp ("Assembly does not exist: " + assemblyPath)
        Directory.SetCurrentDirectory (Path.GetDirectoryName assemblyPath)
        let assembly = Assembly.LoadFrom assemblyPath
        let pluginType =
            if String.IsNullOrWhiteSpace pluginTypeName then
                assembly.GetTypes ()
                |> Array.filter (fun ty -> ty.IsSubclassOf typeof<NuPlugin> && not ty.IsAbstract)
                |> function
                    | [| ty |] -> ty
                    | [||] -> invalidOp ("No NuPlugin type found in " + assemblyPath + ".")
                    | tys -> invalidOp ("Multiple NuPlugin types found; pass pluginType. Types: " + String.Join (", ", Array.map (fun (ty : Type) -> ty.FullName) tys))
            else
                match assembly.GetType pluginTypeName with
                | null -> invalidOp ("Plugin type not found: " + pluginTypeName)
                | ty when not (ty.IsSubclassOf typeof<NuPlugin>) -> invalidOp ("Type is not a NuPlugin: " + pluginTypeName)
                | ty -> ty
        assembly, Activator.CreateInstance pluginType :?> NuPlugin

    let private startGameSession args =
        cleanupSession ()
        Log.setLogSynchronously false
        let assemblyPathArg = requiredString args "assemblyPath"
        let assemblyPath =
            if Path.IsPathRooted assemblyPathArg then Path.GetFullPath assemblyPathArg
            else Path.GetFullPath (Path.Combine (startupDir, assemblyPathArg))
        let pluginTypeName = optionalString args "pluginType" ""
        let _, plugin = loadPlugin assemblyPath pluginTypeName
        Nu.init ()
        Constants.Engine.RunSynchronously <- true
        let windowTitle = optionalString args "windowTitle" (plugin.GetType().Name)
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = windowTitle }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        let windowSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let windowViewport = Viewport.makeWindow1 windowSize
        let geometryViewport = Viewport.makeGeometry windowViewport.Bounds.Size
        match SdlDeps.tryMake worldConfig.SdlConfig worldConfig.Accompanied windowViewport.Outer.Size with
        | Right sdlDeps ->
            let world = World.make (constant None) sdlDeps worldConfig geometryViewport windowViewport plugin
            (Nu.Game.Handle.GetDispatcher world).TrySynchronize (true, true, Nu.Game.Handle, world)
            worldOpt <- Some world
            gameSessionActive <- true
            sdlDepsOpt <- Some (sdlDeps :> IDisposable)
            JsonObject ([ KeyValuePair<string, JsonNode> ("started", JsonValue.Create true)
                          KeyValuePair<string, JsonNode> ("mode", JsonValue.Create "game")
                          KeyValuePair<string, JsonNode> ("assemblyPath", JsonValue.Create assemblyPath)
                          KeyValuePair<string, JsonNode> ("pluginType", JsonValue.Create (plugin.GetType().FullName))
                          KeyValuePair<string, JsonNode> ("gameDispatcher", JsonValue.Create (getTypeName (Nu.Game.Handle.GetDispatcher world)))
                          KeyValuePair<string, JsonNode> ("game", JsonValue.Create (string Nu.Game.Handle))
                          KeyValuePair<string, JsonNode> ("reloadPolicy", JsonValue.Create (if gameHostMode then "game DLL is isolated in a child host process; session_stop terminates that process so rebuilt DLLs load fresh" else "direct in-process game load is not unloadable; parent MCP proxies game sessions to child hosts by default")) ])
        | Left error -> invalidOp error

    let private worldSummary () =
        let world = world ()
        let obj = JsonObject ()
        obj["alive"] <- JsonValue.Create world.Alive
        obj["advancing"] <- JsonValue.Create world.Advancing
        obj["halted"] <- JsonValue.Create world.Halted
        obj["updateTime"] <- JsonValue.Create world.UpdateTime
        obj["clockTime"] <- JsonValue.Create world.ClockTime
        obj["selectedScreen"] <- JsonValue.Create (match World.getSelectedScreenOpt world with Some screen -> string screen | None -> null)
        obj

    let private stepFrames count =
        if gameSessionActive then invalidOp "step_frames is disabled for game sessions because World.runWithoutCleanUp renders and can surface renderer-side Vulkan failures; use state tools without frame stepping or run the game under a renderer-aware diagnostic profile."
        let world = world ()
        let mutable remaining = max 0 count
        let runWhile _ =
            if remaining > 0 then
                remaining <- remaining - 1
                true
            else false
        let nop _ = ()
        World.runWithoutCleanUp runWhile nop nop nop nop nop None world
        worldSummary ()

    let private getProperty args =
        let world = world ()
        let simulant = parseSimulant (requiredString args "simulant")
        let propertyName = requiredString args "property"
        if not (exists simulant world) then invalidOp ("Simulant does not exist: " + string simulant)
        match tryGetProperty propertyName simulant world with
        | Some property -> propertyJson propertyName property
        | None -> invalidOp ("Property '" + propertyName + "' not found on " + string simulant + ".")

    let private inspectMember (value : obj) (memberName : string) =
        if isNull value then invalidOp ("Cannot inspect member '" + memberName + "' on null value.")
        let flags = BindingFlags.Public ||| BindingFlags.Instance
        let ty = value.GetType ()
        let property = ty.GetProperty (memberName, flags)
        if not (isNull property) then property.GetValue value
        else
            let field = ty.GetField (memberName, flags)
            if not (isNull field) then field.GetValue value
            else invalidOp ("Member '" + memberName + "' not found on " + ty.FullName + ".")

    let private inspectProperty args =
        let world = world ()
        let simulant = parseSimulant (requiredString args "simulant")
        let propertyName = requiredString args "property"
        let path = optionalString args "path" ""
        if not (exists simulant world) then invalidOp ("Simulant does not exist: " + string simulant)
        match tryGetProperty propertyName simulant world with
        | Some property ->
            let mutable value = property.PropertyValue
            if not (String.IsNullOrWhiteSpace path) then
                for memberName in path.Split ([|'.'|], StringSplitOptions.RemoveEmptyEntries) do
                    value <- inspectMember value memberName
            JsonObject ([ KeyValuePair<string, JsonNode> ("simulant", JsonValue.Create (string simulant))
                          KeyValuePair<string, JsonNode> ("property", JsonValue.Create propertyName)
                          KeyValuePair<string, JsonNode> ("path", JsonValue.Create path)
                          KeyValuePair<string, JsonNode> ("type", JsonValue.Create (if isNull value then "null" else value.GetType().FullName))
                          KeyValuePair<string, JsonNode> ("value", JsonValue.Create (valueText value)) ])
        | None -> invalidOp ("Property '" + propertyName + "' not found on " + string simulant + ".")

    let private setProperty args =
        let world = world ()
        let simulant = parseSimulant (requiredString args "simulant")
        let propertyName = requiredString args "property"
        let value = requiredString args "value"
        if not (exists simulant world) then invalidOp ("Simulant does not exist: " + string simulant)
        match tryGetProperty propertyName simulant world with
        | Some current ->
            let valueObj = convertStringToType current.PropertyType value
            let property = { PropertyType = current.PropertyType; PropertyValue = valueObj }
            let changed = setPropertyExisting propertyName property simulant world
            let obj = propertyJson propertyName property
            obj["set"] <- JsonValue.Create changed
            obj
        | None -> invalidOp ("Property '" + propertyName + "' not found on " + string simulant + ".")

    let private signalSimulant args =
        let world = world ()
        let simulant = parseSimulant (requiredString args "simulant")
        let signalText = requiredString args "signal"
        if not (exists simulant world) then invalidOp ("Simulant does not exist: " + string simulant)
        let signalObj = symbolFromText signalText :> obj
        signal signalObj simulant world
        JsonObject ([ KeyValuePair<string, JsonNode> ("signaled", JsonValue.Create true)
                      KeyValuePair<string, JsonNode> ("simulant", JsonValue.Create (string simulant))
                      KeyValuePair<string, JsonNode> ("signal", JsonValue.Create signalText) ])

    let private selectScreenTool args =
        let world = world ()
        let simulant = parseSimulant (requiredString args "screen")
        match simulant with
        | :? Screen as screen ->
            if not (screen.GetExists world) then invalidOp ("Screen does not exist: " + string screen)
            World.selectScreen (IdlingState world.GameTime) screen world
            JsonObject ([ KeyValuePair<string, JsonNode> ("selected", JsonValue.Create true)
                          KeyValuePair<string, JsonNode> ("screen", JsonValue.Create (string screen)) ])
        | _ -> invalidArg "screen" "Expected a Screen address such as Game/Gameplay."

    let private publishSymbolEvent args =
        let world = world ()
        let publisher = parseSimulant (optionalString args "publisher" (string Nu.Game.Handle))
        let address = requiredString args "event"
        let data = symbolFromText (optionalString args "data" "()")
        if not (exists publisher world) then invalidOp ("Publisher does not exist: " + string publisher)
        World.publishPlus<Symbol, Simulant> data (stoa<Symbol> address) [] publisher false false world
        JsonObject ([ KeyValuePair<string, JsonNode> ("published", JsonValue.Create true)
                      KeyValuePair<string, JsonNode> ("event", JsonValue.Create address)
                      KeyValuePair<string, JsonNode> ("data", JsonValue.Create (scstring data)) ])

    let private publishKeyboardKeyDown args =
        let world = world ()
        let keyText = requiredString args "key"
        let repeated = optionalBool args "repeated" false
        let key = Enum.Parse<KeyboardKey> (keyText, true)
        let data = { KeyboardKey = key; Repeated = repeated; Down = true }
        let downTrace = EventTrace.debug "Nu.Mcp" "publishKeyboardKeyDown" keyText EventTrace.empty
        World.publishPlus data Nu.Game.Handle.KeyboardKeyDownEvent downTrace Nu.Game.Handle true true world
        let changeTrace = EventTrace.debug "Nu.Mcp" "publishKeyboardKeyChange" keyText EventTrace.empty
        World.publishPlus data Nu.Game.Handle.KeyboardKeyChangeEvent changeTrace Nu.Game.Handle true true world
        JsonObject ([ KeyValuePair<string, JsonNode> ("published", JsonValue.Create true)
                      KeyValuePair<string, JsonNode> ("event", JsonValue.Create (string Nu.Game.Handle.KeyboardKeyDownEvent))
                      KeyValuePair<string, JsonNode> ("key", JsonValue.Create keyText)
                      KeyValuePair<string, JsonNode> ("repeated", JsonValue.Create repeated) ])

    let private subscribeSymbolEvent args =
        let world = world ()
        let address = requiredString args "event"
        let subscriptionId = optionalString args "subscriptionId" (string Gen.id64)
        if subscriptions.ContainsKey subscriptionId then invalidOp ("Subscription already exists: " + subscriptionId)
        let callback (evt : Event<Symbol, Simulant>) (_ : World) =
            eventQueue.Enqueue
                { SubscriptionId = subscriptionId
                  Address = string evt.Address
                  Data = scstring evt.Data
                  Trace = scstring evt.Trace
                  Publisher = string evt.Publisher }
            Cascade
        let unsubscribe = World.subscribePlus<Symbol, Simulant> Gen.id64 callback (stoa<Symbol> address) Nu.Game.Handle world
        subscriptions[subscriptionId] <- unsubscribe
        JsonObject ([ KeyValuePair<string, JsonNode> ("subscribed", JsonValue.Create true)
                      KeyValuePair<string, JsonNode> ("subscriptionId", JsonValue.Create subscriptionId)
                      KeyValuePair<string, JsonNode> ("event", JsonValue.Create address) ])

    let private unsubscribe args =
        let world = world ()
        let subscriptionId = requiredString args "subscriptionId"
        match subscriptions.TryGetValue subscriptionId with
        | true, unsubscribe ->
            unsubscribe world
            subscriptions.Remove subscriptionId |> ignore<bool>
            JsonObject ([ KeyValuePair<string, JsonNode> ("unsubscribed", JsonValue.Create true)
                          KeyValuePair<string, JsonNode> ("subscriptionId", JsonValue.Create subscriptionId) ])
        | false, _ -> invalidOp ("Subscription not found: " + subscriptionId)

    let private drainEvents args =
        let limit = optionalInt args "limit" 100 |> max 0
        let array = JsonArray ()
        let mutable count = 0
        let mutable evt = Unchecked.defaultof<RuntimeEvent>
        while count < limit && eventQueue.TryDequeue &evt do
            let obj = JsonObject ()
            obj["subscriptionId"] <- JsonValue.Create evt.SubscriptionId
            obj["address"] <- JsonValue.Create evt.Address
            obj["data"] <- JsonValue.Create evt.Data
            obj["trace"] <- JsonValue.Create evt.Trace
            obj["publisher"] <- JsonValue.Create evt.Publisher
            array.Add obj
            count <- count + 1
        JsonObject ([ KeyValuePair<string, JsonNode> ("events", array)
                      KeyValuePair<string, JsonNode> ("count", JsonValue.Create count) ])

    let private schema (properties : JsonObject) (required : JsonArray) =
        let obj = JsonObject ()
        obj["type"] <- JsonValue.Create "object"
        obj["properties"] <- properties
        obj["required"] <- required
        obj

    let private stringProp (description : string) =
        JsonObject ([ KeyValuePair<string, JsonNode> ("type", JsonValue.Create "string")
                      KeyValuePair<string, JsonNode> ("description", JsonValue.Create description) ])

    let private intProp (description : string) =
        JsonObject ([ KeyValuePair<string, JsonNode> ("type", JsonValue.Create "integer")
                      KeyValuePair<string, JsonNode> ("description", JsonValue.Create description) ])

    let private tool (name : string) (description : string) (inputSchema : JsonObject) =
        JsonObject ([ KeyValuePair<string, JsonNode> ("name", JsonValue.Create name)
                      KeyValuePair<string, JsonNode> ("description", JsonValue.Create description)
                      KeyValuePair<string, JsonNode> ("inputSchema", inputSchema) ])

    let private requiredArray (names : string list) =
        let array = JsonArray ()
        for name in names do array.Add (jstr name)
        array

    let private props (pairs : (string * JsonNode) list) =
        let obj = JsonObject ()
        for (name, value) in pairs do obj[name] <- value
        obj

    let private toolsList () =
        let tools = JsonArray ()
        tools.Add (tool "session_start_stub" "Start a stub Nu World session for headless event/message/command testing." (schema (JsonObject ()) (JsonArray ())))
        tools.Add (tool "session_start_game" "Start a Nu World session from a built game assembly and NuPlugin." (schema (props ["assemblyPath", stringProp "Built game assembly path"; "pluginType", stringProp "Optional fully-qualified NuPlugin type"; "windowTitle", stringProp "Optional SDL window title"] ) (requiredArray ["assemblyPath"])))
        tools.Add (tool "session_stop" "Clean up the active Nu World session and all event subscriptions." (schema (JsonObject ()) (JsonArray ())))
        tools.Add (tool "world_summary" "Return basic state for the active Nu World." (schema (JsonObject ()) (JsonArray ())))
        tools.Add (tool "step_frames" "Run a bounded number of frames in a stub Nu World. Game sessions are guarded because stepping renders." (schema (props ["count", intProp "Frame count"]) (requiredArray ["count"])))
        tools.Add (tool "get_property" "Read a Game/Screen/Group/Entity property by simulant address and property name." (schema (props ["simulant", stringProp "Address such as Game, Game/Screen, Game/Screen/Group, or Game/Screen/Group/Entity"; "property", stringProp "Property name"]) (requiredArray ["simulant"; "property"])))
        tools.Add (tool "inspect_property" "Read a property or nested public member path from a Game/Screen/Group/Entity value." (schema (props ["simulant", stringProp "Simulant address"; "property", stringProp "Property name"; "path", stringProp "Optional dot-separated public member path such as Run.Player"]) (requiredArray ["simulant"; "property"])))
        tools.Add (tool "set_property" "Set a string-convertible property while preserving Nu property setters and change events." (schema (props ["simulant", stringProp "Simulant address"; "property", stringProp "Property name"; "value", stringProp "Invariant or symbolic value text"]) (requiredArray ["simulant"; "property"; "value"])))
        tools.Add (tool "signal" "Send a symbolic Signal / MMCC Message / MMCC Command to a simulant." (schema (props ["simulant", stringProp "Simulant address"; "signal", stringProp "Symbolic signal text, e.g. StartPlaying or (Move Left)"]) (requiredArray ["simulant"; "signal"])))
        tools.Add (tool "select_screen" "Select a Screen headlessly with IdlingState so selected-only input events can be delivered." (schema (props ["screen", stringProp "Screen address such as Game/Gameplay"]) (requiredArray ["screen"])))
        tools.Add (tool "publish_symbol_event" "Publish a Symbol-valued Nu event from a simulant." (schema (props ["event", stringProp "Event address"; "data", stringProp "Symbolic event payload"; "publisher", stringProp "Optional publisher simulant address"]) (requiredArray ["event"])))
        tools.Add (tool "publish_keyboard_key_down" "Publish a typed Game.KeyboardKeyDownEvent and matching KeyboardKeyChangeEvent." (schema (props ["key", stringProp "KeyboardKey case name such as W, A, S, D, Up, Down, Left, Right, Space, G, Tab, Num1, or Num2"; "repeated", JsonObject ([ KeyValuePair<string, JsonNode> ("type", JsonValue.Create "boolean"); KeyValuePair<string, JsonNode> ("description", JsonValue.Create "Whether this is a repeated key press") ])]) (requiredArray ["key"])))
        tools.Add (tool "subscribe_symbol_event" "Subscribe to a Symbol-valued Nu event and queue matching events for drain_events." (schema (props ["event", stringProp "Event address"; "subscriptionId", stringProp "Optional stable subscription id"]) (requiredArray ["event"])))
        tools.Add (tool "unsubscribe" "Remove an event subscription by id." (schema (props ["subscriptionId", stringProp "Subscription id"]) (requiredArray ["subscriptionId"])))
        tools.Add (tool "drain_events" "Drain queued subscribed events, bounded by limit." (schema (props ["limit", intProp "Maximum events to return"]) (JsonArray ())))
        JsonObject ([ KeyValuePair<string, JsonNode> ("tools", tools) ])

    let private runtimeToolNames =
        HashSet<string>
            ([ "world_summary"
               "step_frames"
               "get_property"
               "inspect_property"
               "set_property"
               "signal"
               "select_screen"
               "publish_symbol_event"
               "publish_keyboard_key_down"
               "subscribe_symbol_event"
               "unsubscribe"
               "drain_events" ],
             StringComparer.Ordinal)

    let private clearGameHostErrors () =
        let mutable ignored = Unchecked.defaultof<string>
        while gameHostErrors.TryDequeue &ignored do ()

    let private gameHostErrorText () =
        let errors = gameHostErrors.ToArray ()
        if errors.Length = 0 then "" else String.Join (Environment.NewLine, errors)

    let private startGameHostProcess () =
        clearGameHostErrors ()
        let assemblyPath = Assembly.GetExecutingAssembly().Location
        let psi = ProcessStartInfo ()
        if assemblyPath.EndsWith (".dll", StringComparison.OrdinalIgnoreCase) then
            psi.FileName <- "dotnet"
            psi.ArgumentList.Add assemblyPath
        else psi.FileName <- assemblyPath
        psi.ArgumentList.Add "--game-host"
        psi.WorkingDirectory <- startupDir
        psi.UseShellExecute <- false
        psi.RedirectStandardInput <- true
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.CreateNoWindow <- true
        let proc = new Process ()
        proc.StartInfo <- psi
        proc.ErrorDataReceived.Add (fun data -> if not (isNull data.Data) then gameHostErrors.Enqueue data.Data)
        if not (proc.Start ()) then invalidOp "Failed to start Nu.Mcp game host process."
        proc.BeginErrorReadLine ()
        proc

    let private nextGameHostRequestId () =
        gameHostRequestId <- gameHostRequestId + 1
        gameHostRequestId

    let private callGameHostTool (name : string) (args : JsonObject) : JsonNode =
        match gameHostProcessOpt with
        | Some proc when not proc.HasExited ->
            let request = JsonObject ()
            request["jsonrpc"] <- JsonValue.Create "2.0"
            request["id"] <- JsonValue.Create (nextGameHostRequestId ())
            request["method"] <- JsonValue.Create "tools/call"
            let paramsObj = JsonObject ()
            paramsObj["name"] <- JsonValue.Create name
            paramsObj["arguments"] <- args.DeepClone ()
            request["params"] <- paramsObj
            proc.StandardInput.WriteLine (request.ToJsonString jsonOptions)
            proc.StandardInput.Flush ()
            let responseLine = proc.StandardOutput.ReadLine ()
            if isNull responseLine then
                let exitText = if proc.HasExited then " exited with code " + string proc.ExitCode else " closed stdout"
                errorResult ("Nu.Mcp game host" + exitText + "." + Environment.NewLine + gameHostErrorText ()) :> JsonNode
            else
                try
                    let responseObj = JsonNode.Parse responseLine :?> JsonObject
                    match tryGetNode "result" responseObj with
                    | Some result -> result.DeepClone ()
                    | None -> errorResult ("Nu.Mcp game host returned no result: " + responseLine) :> JsonNode
                with exn -> errorResult ("Invalid Nu.Mcp game host response: " + responseLine + Environment.NewLine + exn.ToString ()) :> JsonNode
        | Some proc ->
            errorResult ("Nu.Mcp game host already exited with code " + string proc.ExitCode + "." + Environment.NewLine + gameHostErrorText ()) :> JsonNode
        | None -> errorResult "No Nu.Mcp game host process is active. Call session_start_game first." :> JsonNode

    let private disposeGameHostProcess kill =
        match gameHostProcessOpt with
        | Some proc ->
            gameHostProcessOpt <- None
            try
                if kill && not proc.HasExited then
                    try proc.Kill true with _ -> proc.Kill ()
                if not proc.HasExited then proc.WaitForExit 3000 |> ignore<bool>
                if not proc.HasExited then
                    try proc.Kill true with _ -> proc.Kill ()
            with _ -> ()
            try proc.Dispose () with _ -> ()
        | None -> ()

    let private stopGameHostSession () : JsonNode =
        match gameHostProcessOpt with
        | Some proc when not proc.HasExited ->
            let result = callGameHostTool "session_stop" (JsonObject ())
            disposeGameHostProcess true
            result
        | Some _ ->
            let result = errorResult ("Nu.Mcp game host already exited." + Environment.NewLine + gameHostErrorText ()) :> JsonNode
            disposeGameHostProcess false
            result
        | None -> (JsonObject ([ KeyValuePair<string, JsonNode> ("stopped", JsonValue.Create true) ]) |> toolResult) :> JsonNode

    let private startGameHostSession (args : JsonObject) : JsonNode =
        cleanupSession ()
        if Option.isSome gameHostProcessOpt then ignore (stopGameHostSession ())
        let proc = startGameHostProcess ()
        gameHostProcessOpt <- Some proc
        let result = callGameHostTool "session_start_game" args
        match result with
        | :? JsonObject as obj ->
            match tryGetNode "isError" obj with
            | Some (:? JsonValue as value) ->
                match value.TryGetValue<bool> () with
                | true, true -> disposeGameHostProcess true
                | _ -> ()
            | _ -> ()
        | _ -> ()
        result


    let private callTool name (args : JsonObject) : JsonNode =
        try
            if gameHostMode then
                match name with
                | "session_start_stub" -> (startStubSession () |> toolResult) :> JsonNode
                | "session_start_game" -> (startGameSession args |> toolResult) :> JsonNode
                | "session_stop" -> cleanupSession (); gameHostStopRequested <- true; (JsonObject ([ KeyValuePair<string, JsonNode> ("stopped", JsonValue.Create true) ]) |> toolResult) :> JsonNode
                | "world_summary" -> (worldSummary () |> toolResult) :> JsonNode
                | "step_frames" -> (stepFrames (optionalInt args "count" 1) |> toolResult) :> JsonNode
                | "get_property" -> (getProperty args |> toolResult) :> JsonNode
                | "inspect_property" -> (inspectProperty args |> toolResult) :> JsonNode
                | "set_property" -> (setProperty args |> toolResult) :> JsonNode
                | "signal" -> (signalSimulant args |> toolResult) :> JsonNode
                | "select_screen" -> (selectScreenTool args |> toolResult) :> JsonNode
                | "publish_symbol_event" -> (publishSymbolEvent args |> toolResult) :> JsonNode
                | "publish_keyboard_key_down" -> (publishKeyboardKeyDown args |> toolResult) :> JsonNode
                | "subscribe_symbol_event" -> (subscribeSymbolEvent args |> toolResult) :> JsonNode
                | "unsubscribe" -> (unsubscribe args |> toolResult) :> JsonNode
                | "drain_events" -> (drainEvents args |> toolResult) :> JsonNode
                | _ -> errorResult ("Unknown tool: " + name) :> JsonNode
            else
                match name with
                | "session_start_game" -> startGameHostSession args
                | "session_stop" when Option.isSome gameHostProcessOpt -> stopGameHostSession ()
                | _ when Option.isSome gameHostProcessOpt && runtimeToolNames.Contains name -> callGameHostTool name args
                | "session_start_stub" ->
                    if Option.isSome gameHostProcessOpt then ignore (stopGameHostSession ())
                    (startStubSession () |> toolResult) :> JsonNode
                | "session_stop" -> cleanupSession (); (JsonObject ([ KeyValuePair<string, JsonNode> ("stopped", JsonValue.Create true) ]) |> toolResult) :> JsonNode
                | "world_summary" -> (worldSummary () |> toolResult) :> JsonNode
                | "step_frames" -> (stepFrames (optionalInt args "count" 1) |> toolResult) :> JsonNode
                | "get_property" -> (getProperty args |> toolResult) :> JsonNode
                | "inspect_property" -> (inspectProperty args |> toolResult) :> JsonNode
                | "set_property" -> (setProperty args |> toolResult) :> JsonNode
                | "signal" -> (signalSimulant args |> toolResult) :> JsonNode
                | "select_screen" -> (selectScreenTool args |> toolResult) :> JsonNode
                | "publish_symbol_event" -> (publishSymbolEvent args |> toolResult) :> JsonNode
                | "publish_keyboard_key_down" -> (publishKeyboardKeyDown args |> toolResult) :> JsonNode
                | "subscribe_symbol_event" -> (subscribeSymbolEvent args |> toolResult) :> JsonNode
                | "unsubscribe" -> (unsubscribe args |> toolResult) :> JsonNode
                | "drain_events" -> (drainEvents args |> toolResult) :> JsonNode
                | _ -> errorResult ("Unknown tool: " + name) :> JsonNode
        with exn -> errorResult (exn.ToString ()) :> JsonNode

    let private handleRequest (root : JsonObject) =
        let id = match tryGetNode "id" root with Some node -> node | None -> null
        let methodName = requiredString root "method"
        match methodName with
        | "initialize" ->
            let result = JsonObject ()
            result["protocolVersion"] <- JsonValue.Create "2024-11-05"
            result["capabilities"] <- JsonObject ([ KeyValuePair<string, JsonNode> ("tools", JsonObject ()) ])
            result["serverInfo"] <- JsonObject ([ KeyValuePair<string, JsonNode> ("name", JsonValue.Create "Nu.Mcp")
                                                  KeyValuePair<string, JsonNode> ("version", JsonValue.Create "0.1.0") ])
            response id result
        | "tools/list" -> response id (toolsList ())
        | "tools/call" ->
            let paramsObj = root["params"] :?> JsonObject
            let name = requiredString paramsObj "name"
            let args =
                match tryGetNode "arguments" paramsObj with
                | Some (:? JsonObject as args) -> args
                | _ -> JsonObject ()
            response id (callTool name args)
        | methodName when methodName.StartsWith "notifications/" -> ""
        | _ -> errorResponse id -32601 ("Unknown method: " + methodName)

    [<EntryPoint>]
    let main args =
        gameHostMode <- Array.exists ((=) "--game-host") args
        try
            let mutable running = true
            while running do
                let line = Console.In.ReadLine ()
                if isNull line then running <- false
                elif not (String.IsNullOrWhiteSpace line) then
                    try
                        use doc = JsonDocument.Parse line
                        let root = JsonNode.Parse (doc.RootElement.GetRawText ()) :?> JsonObject
                        let output = handleRequest root
                        if not (String.IsNullOrEmpty output) then
                            Console.Out.WriteLine output
                            Console.Out.Flush ()
                        if gameHostMode && gameHostStopRequested then running <- false
                    with exn ->
                        Console.Out.WriteLine (errorResponse null -32700 (exn.ToString ()))
                        Console.Out.Flush ()
            cleanupSession ()
            if not gameHostMode then disposeGameHostProcess true
            0
        with exn ->
            Console.Error.WriteLine (exn.ToString ())
            1
