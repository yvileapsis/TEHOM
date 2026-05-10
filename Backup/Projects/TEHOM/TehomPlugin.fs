namespace Tehom
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type TehomPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList [
            "Splash", Game.SetMyGame Splash
            "Title", Game.SetMyGame Title
            "Credits", Game.SetMyGame Credits
            "Gameplay", fun world ->
                let world = Simulants.Gameplay.SetGameplay Gameplay.initial world
                let world = Game.SetMyGame Gameplay world
                world
            "Rogue", fun world ->
                let world = Simulants.Rogue.SetRogue Rogue.initial world
                let world = Game.SetMyGame Rogue world
                world
        ]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages = [
        Assets.Gui.PackageName
    ]