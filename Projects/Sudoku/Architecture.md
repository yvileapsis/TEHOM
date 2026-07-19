# Nu Architecture Reference

This report captures the architecture of the Nu repository as it exists in this workspace. It is meant to be a future reference for building games on top of Nu, especially when choosing where game-specific behavior should live and how that behavior should interact with the engine.

## Executive Summary

Nu is an F#/.NET game engine organized around a functional simulation core with explicit side-effecting subsystem boundaries. The center of the engine is `World`: a small mutable handle that points at an otherwise persistent `WorldState`. Most engine APIs transform or replace pieces of `WorldState`, while rendering, audio, physics, cursor, SDL, and ImGui are isolated behind subsystem objects.

Game objects are represented as **simulants**. The hierarchy is:

```text
Game
  Screen
    Group
      Entity
        Facets
```

The engine supports two primary application styles:

- **ImSim**, an immediate-mode API where each frame declares screens, groups, entities, and controls from dispatcher `Process` methods.
- **MMCC**, a model-driven API where dispatchers expose `Model`, `Message`, `Command`, and `Content`. Content is synchronized into the world by diffing declarative descriptions.

Both styles share the same runtime: simulants, dispatchers, facets, events, assets, physics, render messages, and editor integration.

## Repository Map

Top-level projects are defined in [Nu.sln](/Users/yvileapsis/Documents/GitHub/Nu/Nu.sln).

Core engine projects:

- [Nu/Nu/Nu.fsproj](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/Nu.fsproj): main F# engine library.
- [Nu/Nu.Math/Nu.Math.csproj](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Math/Nu.Math.csproj): math support.
- [Nu/Nu.Spine/Nu.Spine.csproj](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Spine/Nu.Spine.csproj): Spine animation support.
- [Nu/Nu.Gaia/Nu.Gaia.fsproj](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Gaia/Nu.Gaia.fsproj): Gaia editor.
- [Nu/Nu.Pipe/Nu.Pipe.fsproj](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Pipe/Nu.Pipe.fsproj): tooling pipeline.
- [Nu/Nu.Tests/Nu.Tests.fsproj](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Tests/Nu.Tests.fsproj): NUnit tests.

Game templates:

- [Nu/Nu.Template.ImSim.Game](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.ImSim.Game): full ImSim game template.
- [Nu/Nu.Template.ImSim.Empty](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.ImSim.Empty): minimal ImSim template.
- [Nu/Nu.Template.Mmcc.Game](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.Mmcc.Game): full MMCC game template.
- [Nu/Nu.Template.Mmcc.Empty](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.Mmcc.Empty): minimal MMCC template.

Example projects:

- [Projects/Breakout ImSim](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Breakout ImSim>): Breakout using ImSim.
- [Projects/Breakout Mmcc](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Breakout Mmcc>): Breakout using MMCC.
- [Projects/Blaze Vector ImSim](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Blaze Vector ImSim>) and [Projects/Blaze Vector Mmcc](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Blaze Vector Mmcc>): side-by-side ImSim/MMCC action samples.
- [Projects/Twenty 48](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Twenty 48>): 2D puzzle sample.
- [Projects/Terra Firma](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Terra Firma>): 3D sample.
- [Projects/Sand Box 2d](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Sand Box 2d>), [Projects/Sand Box 3d](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Sand Box 3d>), [Projects/Jump Box](</Users/yvileapsis/Documents/GitHub/Nu/Projects/Jump Box>), [Projects/Nelmish](/Users/yvileapsis/Documents/GitHub/Nu/Projects/Nelmish), and [Projects/Metrics](/Users/yvileapsis/Documents/GitHub/Nu/Projects/Metrics): focused demos, experiments, and diagnostics.

## Core Engine Layers

The compile order in [Nu.fsproj](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/Nu.fsproj) is the best architectural outline:

1. `Core`: addresses, logging, math helpers, time, coroutines, behavior, modifiers, job graph, globals, and constants.
2. `Transform`: viewports, spatial presence/protection, quadtree, octree, transforms.
3. Platform/render foundations: Assimp, OpenGL, SDL, ImGui.
4. Reflection and Overlayer: property reflection, overlays, serialization-like symbolic property descriptions.
5. `EventGraph`: publisher-neutral event subscription and publication.
6. `AssetGraph`: package-based asset discovery, refinement, and loading metadata.
7. Metadata, physics, rendering, audio, cursor, particles, effects, block maps.
8. `World`: game object model, properties, dispatchers, facets, content synchronization, the main loop, subsystems, and public runtime API.

The result is a layered engine where the public game-facing layer is mostly under `Nu/Nu/World`, backed by lower-level service modules.

## World And State

`World` and `WorldState` are defined in [WorldTypes.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldTypes.fs).

`World` is intentionally tiny:

```text
World
  mutable WorldState
```

`WorldState` owns the simulation and subsystem references:

- `EventGraph`: subscriptions, event state, tracing, and event filtering.
- `EntityStates`, `GroupStates`, `ScreenStates`, `GameState`: the simulant state stores.
- `EntityMounts`: parent/mount relationships.
- `Quadtree` and `Octree`: spatial indices for 2D and 3D entities.
- `AmbientState`: time, flags, coroutines, tasklets, SDL dependencies, symbolics, overlayer, timers.
- `Subsystems`: ImGui, 2D physics, 3D physics, renderer process, optional 3D debug renderer, audio player, cursor client.
- `Simulants`: a hierarchy index of registered simulants.
- `EntitiesIndexed`: an entity index keyed by group and dispatcher/type.
- `WorldExtension`: mutable operational data kept out of the main state record for performance.

This design gives Nu an unusual mix of properties:

- Fast imperative access through a mutable `World` handle.
- Snapshot-friendly state because the large simulation record is represented functionally.
- Clear subsystem boundaries because renderer/audio/physics are not arbitrary global services but fields inside `Subsystems`.
- Editor friendliness because state can be inspected, modified, restored, and synchronized.

## Ambient State

`AmbientState` lives in [WorldPrelude.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldPrelude.fs).

It stores cross-cutting runtime state:

- execution flags: `Imperative`, `Accompanied`, `Advancing`, `FramePacing`, `AdvancementCleared`.
- time values: `UpdateDelta`, `UpdateTime`, `ClockDelta`, `ClockTime`, `TickDelta`, `TickTime`, `GameDelta`, `GameTime`, date time values.
- `Coroutines` and `Tasklets`.
- `SdlDepsOpt`.
- `Symbolics`, `Overlayer`, and `Timers`.
- light map render requests.

Important distinction:

- `UpdateTime` is frame/update-count based.
- `ClockTime` and `TickTime` are real elapsed-time based.
- `GameTime` abstracts over static or dynamic frame-rate mode.

For game logic that should be deterministic or turn-like, prefer update-based time. For movement, velocity, and interpolation, use `ClockDelta`/`GameDelta` according to the local engine convention.

## Simulants

Simulants are defined in [Simulant.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/EventGraph/Simulant.fs). A simulant is any participant with a `SimulantAddress`.

The hierarchy is address-based:

```text
Game
Game / "Title"
Game / "Title" / "Gui"
Game / "Title" / "Gui" / "Play"
```

The templates conventionally centralize handles in `Simulants.fs`, for example [Nu.Template.Mmcc.Game/Simulants.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.Mmcc.Game/Simulants.fs). This is worth preserving in future games because it avoids duplicating string literals and makes event bindings safer.

## Properties And Lenses

Nu uses reflected property descriptors plus strongly typed lenses.

The generic lens type in [WorldTypes.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldTypes.fs) supports:

- `Get`, `Set`, `TrySet`.
- `Map`, `TryMap`.
- `ChangeEvent` generation.
- operator forms such as `<~`, `!.`, `+=`, `-=`.

Game code usually exposes custom state as lenses through extension modules. Example from the ImSim template:

```fsharp
type Game with
    member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
    member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
    member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState
```

For future game code:

- Put core gameplay state in typed models for MMCC.
- Put simple mutable simulant properties behind lenses.
- Prefer named extension modules for repeated properties and events.

## Dispatchers And Facets

Dispatchers and facets are the main behavior extension points. Base types are in [WorldTypes.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldTypes.fs).

Dispatchers exist for each simulant level:

- `GameDispatcher`
- `ScreenDispatcher`
- `GroupDispatcher`
- `EntityDispatcher`

They define hooks such as:

- `Register` / `Unregister`
- `TryProcess`
- `PreUpdate`, `Update`, `PostUpdate`
- `Render`
- `Signal`
- `TrySynchronize`
- `Edit`
- model truncation/untruncation for reload/editing workflows

`EntityDispatcher` also defines:

- 2D vs 3D transform interpretation.
- physics, light probe, and light participation flags.
- direct physics registration hooks.
- `RayCast` and inferred attributes.

Facets dynamically augment entity behavior in a composable way. They can participate in:

- registration/unregistration
- physics registration
- update
- render
- ray casting
- inferred attributes
- editor behavior

Built-in dispatchers are in [WorldDispatchers.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldDispatchers.fs). Examples include sprites, GUI controls, body dispatchers, model dispatchers, terrain, sky boxes, lights, tile maps, Spine skeletons, effect entities, and block maps.

Use a dispatcher when the entity has a distinct identity or lifecycle. Use a facet when behavior should be reusable across multiple entity types.

## Plugin System

`NuPlugin` is defined in [WorldTypes.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldTypes.fs). A game provides exactly the kind of plugin the engine needs to discover game-specific dispatchers and configure runtime behavior.

Key plugin responsibilities:

- `InitialPackages`: asset packages loaded at startup, in addition to `Default`.
- `EditModes`: named editor mode callbacks.
- `MakePhysicsEngine2d`: customization point for 2D physics.
- `MakeEmitters`, `GranulatorFns`, `CombinerFns`, `ProcessFns`: particle and block-map extension points.
- `MakeKeyedValues`: global keyed values inserted into the world.
- `PreProcess`, `PerProcess`, `PostProcess`, `ImGuiProcess`, `ImGuiPostProcess`: per-frame callbacks.
- `Invoke`: named plugin callback hook.
- `CleanUp`: user-defined resource cleanup.
- `AllowCodeReload`: controls live code reload.

Late bindings are discovered from plugin assemblies via `Birth<'a>`, which instantiates subclasses of dispatchers and facets with parameterless constructors. This is why game dispatchers and facets usually have empty constructors.

On code reload, `World.updateLateBindings` replaces the plugin and dispatcher/facet instances, updates simulants, and resynchronizes content.

## World Creation

World creation is implemented in [World.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/World.fs).

There are three important paths:

- `World.makePlus`: common constructor once dependencies are already built.
- `World.makeStub`: creates a world with stub physics, renderer, audio, and cursor clients. Tests use this heavily.
- `World.make`: creates a real runtime world from SDL dependencies, asset graph, metadata, plugin assemblies, physics engines, renderer process, audio player, cursor client, spatial trees, and late bindings.

`World.run` is the standard application entry point. Templates use:

```fsharp
Nu.init ()
World.run ignore worldConfig (MyGamePlugin ())
```

The full runtime constructor loads:

- `Assets/AssetGraph.nuag`, if present.
- `Default` plus plugin `InitialPackages`.
- metadata packages.
- plugin-defined dispatchers and facets.
- renderer, audio, and cursor packages.

## Main Loop

The main loop is in [WorldModule2.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldModule2.fs), primarily `World.runWithoutCleanUp`.

Frame order:

1. Engine and plugin pre-process callbacks.
2. Screen transition processing.
3. SDL/input polling and event publication.
4. 3D physics integration, then 2D physics integration.
5. Simulant pre-update.
6. Simulant update.
7. Simulant post-update.
8. Engine and plugin per-process callbacks.
9. Coroutines.
10. Tasklets scheduled for the current time.
11. ImSim cleanup and deferred simulant destruction.
12. Engine and plugin post-process callbacks.
13. Render message generation from simulants.
14. Audio message playback.
15. Frame pacing.
16. ImGui frame.
17. Renderer message submission and swap request.
18. ImGui post-process callbacks.
19. Time update and `TimeUpdateEvent` publication.
20. Recursive next frame.

Important consequences:

- Physics events are available before normal update.
- End-of-frame destruction happens after tasklets and before post-process/render.
- Rendering is message-based. Simulants enqueue render messages, and the renderer process consumes them later.
- Audio is also message-based.
- `TimeUpdateEvent` is published after time advances, not at the beginning of the frame.

## Event Graph

The event system is implemented in [EventGraph.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/EventGraph/EventGraph.fs) and surfaced through `World` functions.

Core concepts:

- `Handling = Resolve | Cascade`.
- Subscriptions are publisher-neutral: listeners can subscribe before publishers exist.
- Event addresses contain an `"Event"` separator.
- Wildcards and ellipses allow generalized subscriptions.
- Events are sorted by subscriber priority before publication.
- Event tracing can be enabled through constants/config.

Events are defined in [WorldEvents.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldEvents.fs). Built-ins cover lifecycle, property changes, screen selection, input, physics integration, body events, gravity changes, GUI controls, Spine triggers, asset reloads, and more.

For game code:

- Use strongly typed events and payload records/unions.
- Keep event handlers small.
- In MMCC, prefer binding events to messages or commands through content definitions.
- In ImSim, use `World.doSubscription*` helpers when immediate-mode state wants event results for the current frame.

## ImSim Architecture

ImSim is defined primarily in [WorldImSim.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldImSim.fs) and the ImSim dispatcher classes in [WorldModule2.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldModule2.fs).

ImSim works by declaring the desired simulant tree each frame inside dispatcher `Process` methods.

Core argument operators:

- `|=`: initializing static arg, applied only when the simulant is first initialized.
- `.=`: reinitializing static arg, also re-applied on code reload.
- `@=`: dynamic arg, applied every frame.

Core context functions:

- `World.beginGame` / `World.endGame`
- `World.beginScreen` / `World.endScreen`
- `World.beginGroup` / `World.endGroup`
- `World.doEntity`, `World.doButton`, and related helpers

The engine tracks `ContextImSim`, `DeclaredImSim`, `SimulantsImSim`, and `SubscriptionsImSim` in `WorldExtension`. Unused ImSim simulants can be cleaned up, and code reload can force reinitialization behavior.

The ImSim template in [MyGame.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.ImSim.Game/MyGame.fs) shows the pattern:

- Game state is a custom property on `Game`.
- The game dispatcher inherits `GameDispatcherImSim`.
- `Process` declares screens.
- Buttons directly mutate state or exit the world.
- Screen selection results drive state transitions.

Use ImSim for fast, direct game declaration, editor-friendly immediate UI, and games where frame-by-frame structural declaration feels natural.

## MMCC Architecture

MMCC is implemented through generic dispatcher classes in [WorldModule2.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldModule2.fs) and the content synchronization system in [WorldContent.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldContent.fs).

MMCC stands for:

- `Model`: immutable state for a game, screen, group, or entity.
- `Message`: model-transforming signal.
- `Command`: world-transforming side effect.
- `Content`: declarative child tree and property/event definitions.

Generic dispatchers include:

- `GameDispatcher<'model, 'message, 'command>`
- `ScreenDispatcher<'model, 'message, 'command>`
- `GroupDispatcher<'model, 'message, 'command>`
- `EntityDispatcher<'model, 'message, 'command>`

The dispatcher:

- initializes and stores the model in the simulant's generic model property.
- handles physics, edit, and arbitrary signals by routing them through message/command logic.
- derives content from the current model.
- synchronizes content against previous content.
- supports fallback, truncation, and untruncation for reload/editor scenarios.

The MMCC template in [MyGame.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.Mmcc.Game/MyGame.fs) and [Gameplay.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.Mmcc.Game/Gameplay.fs) shows the pattern:

- Define a model union/record.
- Define messages and commands.
- Bind events to messages/commands in `Definitions`.
- Return new model values from `Message`.
- Perform world side effects in `Command`.
- Produce screens, groups, entities, and controls in `Content`.

Use MMCC when game state is important enough to deserve a clear, testable model, especially for larger games, stateful screens, undoable/editor-friendly behavior, or complex workflows.

## Content Synchronization

`Content` in [WorldContent.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldContent.fs) is the declarative bridge between models and world objects.

It synchronizes:

- event signals and event handlers.
- property contents.
- child screens, groups, and entities.
- file-backed content, such as `nugroup` and `nuentity` files.

The synchronizer diffs old and new content:

- Added child content creates simulants.
- Removed child content destroys simulants.
- Matching child content recurses and updates.
- Dispatcher name changes force replacement.
- Event subscriptions retain subscription ids where possible.

This matters for future architecture because content should be treated as a declarative source of truth. Avoid manually creating/destroying content-owned children from unrelated code. Let the owning dispatcher produce content and let the engine synchronize it.

## Assets

Assets are described by `AssetGraph` in [AssetGraph.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/AssetGraph/AssetGraph.fs).

Concepts:

- Every asset belongs to a package.
- Packages are the unit of load/unload for renderers, audio, cursor, and metadata.
- Asset descriptors can name individual assets or scan directories by extension.
- Refinements include PSD-to-PNG and block compression.
- Default package scanning is built in for `Assets/Default`.

Game projects typically include:

- `AssetGraph.nuag`
- `Assets/Default`
- `Assets/Gui`
- `Assets/Gameplay`
- generated F# constants in `Assets.fs`

Plugin `InitialPackages` should list packages needed at startup. Load/unload larger packages explicitly when entering and leaving heavier game modes to avoid runtime stalls.

## Rendering

Rendering is message-oriented. The public world helpers are in [WorldRender.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldRender.fs), while the renderer process is in [RendererProcess.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/Render/RendererProcess.fs).

World APIs enqueue:

- 3D render messages.
- 2D render messages.
- ImGui render messages.
- fast-path sprite and model operations.
- package load/unload/reload messages.

At the end of the frame, `RendererProcess.SubmitMessages` receives:

- 3D frustums and eye values.
- 2D eye center and size.
- current window and geometry viewports.
- ImGui draw data.

Renderer implementations include inline and threaded modes, selected by `Constants.Engine.RunSynchronously`.

Rendering depends heavily on spatial queries:

- 2D uses the quadtree.
- 3D uses the octree.
- presence values distinguish interior, exterior, imposter, and omnipresent participation.
- light probes, lights, shadows, and culling use 3D element flags.

## Physics

Physics is abstracted through `PhysicsEngine` and world helpers in [WorldPhysics.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldPhysics.fs).

Current runtime defaults:

- 2D: plugin-created engine, defaulting to `Box2dNetPhysicsEngine`.
- 3D: `JoltPhysicsEngine`.
- Tests/stubs: `StubPhysicsEngine`.

Physics messages create/destroy bodies and joints, apply impulses/forces, update velocities, perform casts, and configure gravity. Integration messages are published as events and then applied to entities through dispatcher/facet hooks.

Body event families include:

- body adding/removing
- penetration
- explicit separation
- implicit separation
- transform updates
- joint break events

Game code should handle both explicit and implicit separation, as the engine comments call out that physics engines may not raise separation until a later frame.

## Audio And Cursor

Audio helpers are in [WorldAudio.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldAudio.fs).

Audio is message-based:

- play sounds
- play/fade/stop songs
- set master/sound/song volumes
- load/unload/reload audio packages

Cursor helpers are isolated through `CursorClient`, with SDL and stub implementations. Cursor assets are package-loaded similarly to audio and render assets.

## SDL And ImGui

SDL setup is controlled through `SdlConfig` and `WorldConfig`. A standard game program:

1. Sets current directory to `AppContext.BaseDirectory`.
2. Calls `Nu.init`.
3. Builds `SdlWindowConfig`, `SdlConfig`, and `WorldConfig`.
4. Calls `World.run`.

Input polling happens in the main loop and publishes engine events onto `Game.Handle` addresses. ImGui is processed near the end of the frame after normal simulation/render message generation and before final renderer submission.

Gaia/editor support is represented by `Accompanied = true`, edit contexts, edit modes, edit deferrals, ImGui editor hooks, and reloadable late bindings.

## Spatial Indexing

2D and 3D spatial indexing are separate:

- `Quadtree` stores 2D quadelements.
- `Octree` stores 3D octelements.

Tests in [QuadtreeTests.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Tests/QuadtreeTests.fs) and [OctreeTests.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Tests/OctreeTests.fs) show expected behavior around power-of-two bounds, presence filtering, view queries, light probes, and removal.

Entity transforms, presence, visibility, static flags, light flags, and bounds determine how entities participate in spatial queries.

## Coroutines And Tasklets

Coroutines and tasklets live in `AmbientState`.

Coroutines:

- are launched from `world.Launcher` or `world.LauncherWhile`.
- can sleep, pass, loop, recurse, and cancel.
- are processed after per-process callbacks and before tasklets.

Tasklets:

- are scheduled operations tied to a simulant.
- run near end-of-frame.
- are used internally to avoid unsafe timing interactions, such as changing advancement state during ImSim processing.

The coroutine tests in [CoroutineTests.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Tests/CoroutineTests.fs) are a useful behavior reference.

## Editor And Reload Model

Nu is built to support live editing:

- `WorldState` can be snapshotted and restored.
- `EditOperation` values represent undoable editor actions.
- `NuPlugin.EditModes` exposes game-specific editor state changes.
- `Overlayer` and reflection attach default/custom properties.
- Code reload uses `World.updateLateBindings`.
- Content can be re-synchronized after dispatcher/facet changes.
- Models can be truncated/untruncated to survive reloads or editor state transitions.

When adding gameplay code, assume it may run under Gaia. Check `world.Accompanied` or `world.Unaccompanied` around behavior that should only happen in shipped/runtime mode, such as exiting the app from a button.

## Testing Model

Tests use `World.makeStub` to avoid real SDL/render/audio dependencies. This is the right pattern for future engine and gameplay tests when possible.

Common test setup:

```fsharp
Nu.init ()
let world = World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
let result = World.runWithCleanUp runWhile preProcess perProcess postProcess ignore ignore true world
```

Use stub worlds for:

- address and event behavior.
- content synchronization.
- dispatcher update logic.
- coroutine/tasklet behavior.
- spatial indexing.
- deterministic game logic.

Use integration tests only when SDL, rendering, or real platform subsystems must be exercised.

## Building A Game On Top

A new game should generally follow the template shape:

```text
Program.fs
Assets.fs
Simulants.fs
Events.fs
<GameName>.fs
<GameName>Plugin.fs
Gameplay.fs or feature-specific screen/entity modules
Assets/
  Default/
  Gui/
  Gameplay/
AssetGraph.nuag
Overlayer.nuol
App.config
```

Recommended ownership:

- `Program.fs`: startup only.
- `Assets.fs`: typed asset constants and package names.
- `Simulants.fs`: stable handles for important game objects.
- `Events.fs`: game-specific typed events.
- Plugin file: initial packages, edit modes, plugin hooks, external integrations.
- Top-level game dispatcher: screen state and screen routing.
- Screen dispatchers: screen-local model and content.
- Entity dispatchers/facets: reusable simulation pieces.

For larger games, prefer MMCC for major screens and systems because the model/message/command boundary gives cleaner future maintenance. ImSim remains excellent for editor-facing tools, quick direct UI, prototypes, and local interactive declarations.

## Practical Rules For Future Work

- Keep game state explicit. If it drives gameplay, put it in a typed model or typed simulant property.
- Keep asset package boundaries intentional. Startup packages should be small enough to load comfortably.
- Use `Simulants.fs` handles for anything referenced by multiple modules.
- Use `Events.fs` for game-specific event addresses and payloads.
- Use dispatchers for primary behavior and facets for reusable capabilities.
- Do not manually mutate children owned by MMCC content from elsewhere. Change the model and let content synchronize.
- In ImSim, use `|=`, `.=` and `@=` deliberately. Treat `@=` as every-frame truth.
- Gate runtime-only effects with `world.Unaccompanied` when editor mode should not trigger them.
- Prefer `World.makeStub` for logic tests.
- Prefer package load/unload APIs before heavy gameplay transitions.
- Treat `World.runWithoutCleanUp` order as contract when debugging event timing.

## High-Value Files To Revisit

- [Nu/Nu/World/WorldTypes.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldTypes.fs): core types, dispatchers, facets, plugin, world state.
- [Nu/Nu/World/World.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/World.fs): initialization, world construction, run entry points, late binding updates.
- [Nu/Nu/World/WorldModule2.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldModule2.fs): main loop, screen transitions, input, ImSim/MMCC dispatcher implementations.
- [Nu/Nu/World/WorldImSim.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldImSim.fs): immediate-mode API.
- [Nu/Nu/World/WorldContent.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldContent.fs): MMCC content synchronization.
- [Nu/Nu/World/WorldDispatchers.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldDispatchers.fs): built-in entity dispatchers.
- [Nu/Nu/EventGraph/EventGraph.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/EventGraph/EventGraph.fs): event subscription and publication.
- [Nu/Nu/AssetGraph/AssetGraph.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/AssetGraph/AssetGraph.fs): packages and asset refinement.
- [Nu/Nu/World/WorldRender.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldRender.fs): render message API.
- [Nu/Nu/World/WorldPhysics.fs](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu/World/WorldPhysics.fs): physics message and query API.
- [Nu/Nu.Template.Mmcc.Game](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.Mmcc.Game): best starting point for a structured game.
- [Nu/Nu.Template.ImSim.Game](/Users/yvileapsis/Documents/GitHub/Nu/Nu/Nu.Template.ImSim.Game): best starting point for immediate-mode game structure.

## Mental Model

Nu is easiest to reason about as four cooperating systems:

1. **Declarative simulation state**: game/screen/group/entity states inside `WorldState`.
2. **Behavior bindings**: dispatchers, facets, plugin hooks, lenses, and event subscriptions.
3. **Frame pipeline**: input, physics, updates, deferred work, rendering/audio submission, ImGui, time.
4. **External services**: SDL, OpenGL renderer process, physics engines, audio player, cursor client, metadata, assets.

When building games, most code should live in the first two systems and talk to the last two only through `World` APIs. That keeps gameplay understandable, testable, editor-compatible, and aligned with the engine's functional core.
