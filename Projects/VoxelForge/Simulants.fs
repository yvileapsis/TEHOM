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

    let WorldGeneration = Game / "WorldGeneration"
    let WorldGenerationEnvironment = WorldGeneration / "Environment"
    let WorldGenerationSkyBox = WorldGenerationEnvironment / "SkyBox"
    let WorldGenerationSunLight = WorldGenerationEnvironment / "SunLight"
    let WorldGenerationLightProbe = WorldGenerationEnvironment / "LightProbe"
    let WorldGenerationGui = WorldGeneration / "Gui"
    let WorldGenerationTitle = WorldGenerationGui / "Title"
    let WorldGenerationStatus = WorldGenerationGui / "Status"
    let WorldGenerationProgress = WorldGenerationGui / "Progress"

    let Gameplay = Game / "Gameplay"
    let GameplayGui = Gameplay / "Gui"
    let GameplayPauseBackdrop = GameplayGui / "PauseBackdrop"
    let GameplayPausePanel = GameplayGui / "PausePanel"
    let GameplayPauseTitle = GameplayPausePanel / "Title"
    let GameplayResume = GameplayPausePanel / "Resume"
    let GameplayMainMenu = GameplayPausePanel / "MainMenu"
    let GameplayQuit = GameplayPausePanel / "Quit"
    let GameplayScene = Gameplay / "Scene"
    let GameplayEnvironment = GameplayScene / "GeneratedEnvironment"
    let GameplaySkyBox = GameplayScene / "GeneratedSkyBox"
    let GameplaySunLight = GameplayScene / "GeneratedSunLight"
    let GameplayLightProbe = GameplayScene / "GeneratedLightProbe"
    let VoxelLevelChunk x y z = GameplayScene / ("VoxelLevel" + string x + "_" + string y + "_" + string z)
    let GameplayPlayer = GameplayScene / "GameplayPlayer"
    let AimBlockHighlightFace index = GameplayScene / ("AimBlockHighlightFace" + string index)
    let AimBlockHighlightLight = GameplayScene / "AimBlockHighlightLight"
    let BluePortalAperture = GameplayScene / "BluePortalAperture"
    let OrangePortalAperture = GameplayScene / "OrangePortalAperture"
