namespace Psychocrawl
open System
open Nu
open Psychocrawl

type Psychocrawl =
    | Gameplay
    | FontBenchmarkMode

    static member initial =
        if String.Equals (Environment.GetEnvironmentVariable "PSYCHOCRAWL_FONT_BENCHMARK", "1", StringComparison.Ordinal)
        then FontBenchmarkMode
        else Gameplay

type PsychocrawlMessage =
    | ShowGameplay
    | ShowFontBenchmark
    | GameKeyPressed of KeyboardKeyData
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

    override this.Definitions (psychocrawl, _) =
        [Game.DesiredScreen :=
            match psychocrawl with
            | Gameplay -> Desire Simulants.Gameplay
            | FontBenchmarkMode -> Desire Simulants.FontBenchmark
         Game.KeyboardKeyDownEvent =|> fun evt -> GameKeyPressed evt.Data]

    override this.Message (psychocrawl, message, _, _) =
        match message with
        | ShowGameplay -> just Gameplay
        | ShowFontBenchmark -> just FontBenchmarkMode
        | GameKeyPressed data ->
            match psychocrawl, data.KeyboardKey with
            | Gameplay, KeyboardKey.F10 -> just FontBenchmarkMode
            | FontBenchmarkMode, KeyboardKey.F10
            | FontBenchmarkMode, KeyboardKey.Escape -> just Gameplay
            | _ -> just psychocrawl

    override this.Command (_, command, _, world) =
        match command with
        | Exit -> if world.Unaccompanied then World.exit world

    override this.Content (psychocrawl, _) =
        let gameplay = Content.screen<GameplayDispatcher> Simulants.Gameplay.Name Vanilla [] []
        let fontBenchmark = Content.screen<FontBenchmarkDispatcher> Simulants.FontBenchmark.Name Vanilla [] []
        match psychocrawl with
        | Gameplay -> [gameplay; fontBenchmark]
        | FontBenchmarkMode -> [fontBenchmark; gameplay]
