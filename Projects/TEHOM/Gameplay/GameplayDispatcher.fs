namespace Tehom

open Prime
open Nu

[<AutoOpen>]
module GameplayDispatcher =

    // this extends the Screen API to expose the above Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place. Note that we just use the
    // empty Command type because there are no commands needed for this template.
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, Command> ({
            Serialization.loadFromFile Gameplay.makeDefault with State = Gameplay.Quitting
        })

        // here we define the screen's properties and event handling
        override this.Initialize (_, _) = [
            Screen.UpdateEvent => Update
            Screen.DeselectingEvent => FinishQuitting
        ]

        // here we handle the above messages
        override this.Message (gameplay, message, _, _) =
            match message with
            | Update ->
                just { gameplay with Time = inc gameplay.Time }
            | SetDisplayedString str ->
                just { gameplay with Display = str}
            | SetDisplayedStringToActorDescription actorID ->
                let description =
                    $"{Actions.canSense actorID gameplay.Compositions gameplay.Actors}"
(*                    match Map.tryFind actorID gameplay.Actors with
                    | Some actor -> actor.getDescription
                    | _ -> $"did not find %A{actorID}, weird!"
*)
                just { gameplay with Display = description }
            | Save ->
                Serialization.saveToFile gameplay
                just gameplay
            | Load ->
                let gameplay = Serialization.loadFromFile gameplay
                just gameplay

            | StartQuitting ->
                just { gameplay with State = Gameplay.Quitting }
            | FinishQuitting ->
                just { gameplay with State = Gameplay.Quit }

        // here we describe the content of the game including the level, the hud, and the player
        override this.Content (gameplay, _) = [// the gui group

            GameplayGui.Gui gameplay

            // the scene group while playing or quitting
            match gameplay.State with
            | Gameplay.Playing | Gameplay.Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] []
            | Gameplay.Quit -> ()]

type GameplayDispatcher = GameplayDispatcher.GameplayDispatcher