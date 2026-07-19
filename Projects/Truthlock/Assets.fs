namespace Truthlock
open System
open Prime
open Nu

// Asset constants used by Truthlock. LocalFan is intentionally optional and ignored by git.
[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let PackageDirectoryPath = "Assets/Gui"

    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let PackageDirectoryPath = "Assets/Gameplay"

    [<RequireQualifiedAccess>]
    module LocalFan =

        let PackageName = "LocalFan"
        let PackageDirectoryPath = "Assets/LocalFan"
