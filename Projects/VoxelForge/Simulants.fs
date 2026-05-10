namespace VoxelForge
open System
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    let Splash = Game / "Splash"

    let Title = Game / "Title"
    let TitleGui = Title / "Gui"
    let TitlePlay = TitleGui / "Play"
    let TitleCredits = TitleGui / "Credits"
    let TitleExit = TitleGui / "Exit"

    let Credits = Game / "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"

    let Gameplay = Game / "Gameplay"
    let GameplayGui = Gameplay / "Gui"
    let GameplayQuit = GameplayGui / "Quit"
    let GameplayScene = Gameplay / "Scene"
    let VoxelLevelChunk x y z = GameplayScene / ("VoxelLevel" + string x + "_" + string y + "_" + string z)
    let RayPickMarker = GameplayScene / "RayPickMarker"
