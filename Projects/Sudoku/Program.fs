namespace Sudoku
open System
open System.IO
#if SUDOKU_IOS
open SDL
open FSharp.NativeInterop
open System.Runtime.InteropServices
#endif
open Nu
module Program =

    // this the entry point for your Nu application
    let private run () =

        // this points the current working directory at the bundled game assets
        let baseDirectory = AppContext.BaseDirectory
        let nestedAssetDirectory = Path.Combine (baseDirectory, "refinement-out", "net10.0-ios")
        let workingDirectory =
            if Directory.Exists (Path.Combine (baseDirectory, "Assets")) then baseDirectory
            elif Directory.Exists (Path.Combine (nestedAssetDirectory, "Assets")) then nestedAssetDirectory
            else baseDirectory
        Directory.SetCurrentDirectory workingDirectory

        // this initializes Nu before other Nu code is run
        Nu.init ()

        // this specifies the window configuration used to display the game
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Sudoku" }

        // this specifies the configuration of the game engine's use of SDL
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }

        // this specifies the world config using the above SDL config
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

        // this runs the engine with the given config and plugin, starting the game
        World.run ignore worldConfig (SudokuPlugin ())

#if SUDOKU_IOS
    // SDL_RunApp is required on iOS so SDL can own the application lifecycle.
    type private SdlMain = delegate of argc : int * argv : byte nativeptr nativeptr -> int

    let private sdlMain =
        SdlMain (fun _ _ -> run ())
#endif

    let [<EntryPoint; STAThread>] main _ =
#if SUDOKU_IOS
        Log.init None // disable Nu's default file log because the iOS app bundle is read-only.
        SDL3.SDL_RunApp (0, NativePtr.nullPtr, Marshal.GetFunctionPointerForDelegate<_> sdlMain, 0n)
#else
        run ()
#endif
