namespace Tehom

open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // these are assets from the Gui package.
    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let TitleGroupFilePath = "Assets\\Gui\\Title.nugroup"
        let CreditsGroupFilePath = "Assets\\Gui\\Credits.nugroup"
        let ClearSans12Font = asset<Font> PackageName "ClearSansFont"
        let MontSerratFont = asset<Font> PackageName "MontSerratFont"


    // these are assets from the Gameplay package.
    [<RequireQualifiedAccess>]
    module Gameplay =
        
        let PackageName = "Gameplay"
 
    // these are assets from the Entities package.
    [<RequireQualifiedAccess>]
    module Actors = 

        let PackageName = "Actors"
        let ActorsDefault = "Assets\\Actors\\default.yaml"

        let ActorsContentYaml = "Assets\\Actors\\content.yaml"

        let ActorsContentTehom = "Assets\\Actors\\content.tehom"

        let ActorsContent = "Assets\\Actors\\"

        let Directory = __SOURCE_DIRECTORY__