namespace Truthlock
open System
open System.IO
open Nu

module Program =

    let [<EntryPoint; STAThread>] main _ =

        Directory.SetCurrentDirectory AppContext.BaseDirectory

        Nu.init ()

        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Truthlock" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

        World.run ignore worldConfig (TruthlockPlugin ())
