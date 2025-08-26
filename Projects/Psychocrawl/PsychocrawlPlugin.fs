namespace Psychocrawl
open System
open Nu
open Psychocrawl

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type PsychocrawlPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList [
             "Splash", Game.SetPsychocrawl Splash
             "Title", Game.SetPsychocrawl Title
             "Credits", Game.SetPsychocrawl Credits
             "Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetPsychocrawl Gameplay world
        ]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages = [
        Assets.Gui.PackageName
        Assets.Gameplay.PackageName
    ]