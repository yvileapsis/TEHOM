namespace Psychocrawl
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"

    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let SectionsDirectoryPath = "Assets/Gameplay/Sections"
        let Unscii8 = asset<Font> PackageName "unscii-8"
        let Unscii16 = asset<Font> PackageName "unscii-16"
        let Unscii16Full = asset<Font> PackageName "unscii-16-full"
        let DejaVuSansMono = asset<Font> PackageName "dejavu-sans-mono"
        let Unscii8Mtsdf = asset<MsdfFont> PackageName "Unscii8Mtsdf"
        let Unscii16Mtsdf = asset<MsdfFont> PackageName "Unscii16Mtsdf"
        let Unscii16FullMtsdf = asset<MsdfFont> PackageName "Unscii16FullMtsdf"
        let DejaVuSansMonoMtsdf = asset<MsdfFont> PackageName "DejaVuSansMonoMtsdf"
