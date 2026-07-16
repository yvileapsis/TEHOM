namespace Psychocrawl
open System
open System.IO
open Nu

module Program =

    let [<EntryPoint; STAThread>] main args =
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        if args |> Array.contains "--font-benchmark" then
            Environment.SetEnvironmentVariable ("PSYCHOCRAWL_FONT_BENCHMARK", "1")
        Nu.init ()
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Psychocrawl Prototype" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        World.run worldConfig (PsychocrawlPlugin ())
