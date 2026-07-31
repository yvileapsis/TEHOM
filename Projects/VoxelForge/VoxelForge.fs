namespace VoxelForge
open System
open Prime
open Nu

type VoxelForgeMessage =
    | ShowTitle
    | ShowCredits
    | ShowWorldGeneration
    | FinishWorldGeneration of GeneratedWorldPackage
    interface Message

type VoxelForgeCommand =
    | Exit
    interface Command

type VoxelForgeDispatcher () =
    inherit GameDispatcher<VoxelForge, VoxelForgeMessage, VoxelForgeCommand> (VoxelForge.initial)

    override this.Definitions (voxelForge, _) =
        [Game.DesiredScreen :=
            match voxelForge.Mode with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | Credits -> Desire Simulants.Credits
            | WorldGeneration -> Desire Simulants.WorldGeneration
            | Gameplay -> Desire Simulants.Gameplay
         if voxelForge.Mode = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
         Simulants.TitleCredits.ClickEvent => ShowCredits
         Simulants.TitlePlay.ClickEvent => ShowWorldGeneration
         Simulants.TitleExit.ClickEvent => Exit
         Simulants.CreditsBack.ClickEvent => ShowTitle
         Simulants.WorldGeneration.WorldGeneratedEvent =|> fun evt -> FinishWorldGeneration evt.Data
         Simulants.Gameplay.MainMenuEvent => ShowTitle]

    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just VoxelForge.title
        | ShowCredits -> just VoxelForge.credits
        | ShowWorldGeneration -> just VoxelForge.worldGeneration
        | FinishWorldGeneration package -> just (VoxelForge.gameplay (Some package))

    override this.TruncateModel voxelForge =
        { voxelForge with GeneratedWorldPackageOpt = None }

    override this.UntruncateModel (current, incoming) =
        { incoming with GeneratedWorldPackageOpt = current.GeneratedWorldPackageOpt }

    override this.Command (_, command, _, world) =
        match command with
        | Exit -> if world.Unaccompanied then World.exit world

    override this.Content (_, _) =
        [Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" [] []
         Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" [] []
         Content.screen<WorldGenerationDispatcher> Simulants.WorldGeneration.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
         Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]
