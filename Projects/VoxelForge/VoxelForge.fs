namespace VoxelForge
open System
open Prime
open Nu
open VoxelForge

type VoxelForge =
    | Splash
    | Title
    | Credits
    | Gameplay

type VoxelForgeMessage =
    | ShowTitle
    | ShowCredits
    | ShowGameplay
    interface Message

type VoxelForgeCommand =
    | Exit
    interface Command

[<AutoOpen>]
module VoxelForgeExtensions =
    type Game with
        member this.GetVoxelForge world = this.GetModelGeneric<VoxelForge> world
        member this.SetVoxelForge value world = this.SetModelGeneric<VoxelForge> value world
        member this.VoxelForge = this.ModelGeneric<VoxelForge> ()

type VoxelForgeDispatcher () =
    inherit GameDispatcher<VoxelForge, VoxelForgeMessage, VoxelForgeCommand> (Splash)

    override this.Definitions (voxelForge, _) =
        [Game.DesiredScreen :=
            match voxelForge with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | Credits -> Desire Simulants.Credits
            | Gameplay -> Desire Simulants.Gameplay
         if voxelForge = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
         Simulants.TitleCredits.ClickEvent => ShowCredits
         Simulants.TitlePlay.ClickEvent => ShowGameplay
         Simulants.TitleExit.ClickEvent => Exit
         Simulants.CreditsBack.ClickEvent => ShowTitle
         Simulants.Gameplay.QuitEvent => ShowTitle]

    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just Title
        | ShowCredits -> just Credits
        | ShowGameplay -> just Gameplay

    override this.Command (_, command, _, world) =
        match command with
        | Exit -> if world.Unaccompanied then World.exit world

    override this.Content (_, _) =
        [Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" [] []
         Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" [] []
         Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]
