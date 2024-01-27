namespace Tehom

open Nu
open FParsec

open FSharp.Formatting.Markdown
open FSharp.Formatting.Common

module Testing =
    let document =
        """
# F# Hello world
Hello world in [F#](http://fsharp.net) looks like this:

    printfn "Hello world!"

For more see [fsharp.org][fsorg].

  [fsorg]: http://fsharp.org "The F# organization." """

    let parsed = Markdown.Parse(document)

    parsed.Paragraphs.Head