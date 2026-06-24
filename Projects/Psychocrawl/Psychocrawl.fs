namespace Psychocrawl
open Nu
open Psychocrawl

type Psychocrawl =
    | Gameplay
    static member val initial = Gameplay

type PsychocrawlMessage =
    | ShowGameplay
    interface Message

type PsychocrawlCommand =
    | Exit
    interface Command

[<AutoOpen>]
module PsychocrawlExtensions =
    type Game with
        member this.GetPsychocrawl world = this.GetModelGeneric<Psychocrawl> world
        member this.SetPsychocrawl value world = this.SetModelGeneric<Psychocrawl> value world
        member this.Psychocrawl = this.ModelGeneric<Psychocrawl> ()

type PsychocrawlDispatcher () =
    inherit GameDispatcher<Psychocrawl, PsychocrawlMessage, PsychocrawlCommand> (Psychocrawl.initial)

    override this.Definitions (_, _) =
        [Game.DesiredScreen := Desire Simulants.Gameplay]

    override this.Message (_, message, _, _) =
        match message with
        | ShowGameplay -> just Gameplay

    override this.Command (_, command, _, world) =
        match command with
        | Exit -> if world.Unaccompanied then World.exit world

    override this.Content (_, _) =
        [Content.screen<GameplayDispatcher> Simulants.Gameplay.Name Vanilla [] []]
