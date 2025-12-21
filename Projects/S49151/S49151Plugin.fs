namespace S49151
open System
open Nu
open S49151

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type S49151Plugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList [
            "Splash", Game.SetS49151 Splash
            "Title", Game.SetS49151 Title
            "Credits", Game.SetS49151 Credits
            "Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetS49151 Gameplay world
        ]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages = [
        Assets.Gui.PackageName
        Assets.Gameplay.PackageName
    ]