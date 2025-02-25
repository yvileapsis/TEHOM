﻿namespace Tehom
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // these are assets from the Gui package. Note that we don't actually have any assets here yet, but they can be
    // added to the existing package at your leisure!
    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let TitleGroupFilePath = "Assets\\Gui\\Title.nugroup"
        let CreditsGroupFilePath = "Assets\\Gui\\Credits.nugroup"
        let ClearSansFont = asset<Font> PackageName "ClearSans"
        let MontSerratFont = asset<Font> PackageName "Montserrat-Regular"
        let MonaspaceFont = asset<Font> PackageName "MonaspaceKrypton-Regular"
        let SpaceMonoFont = asset<Font> PackageName "SpaceMono-Regular"

    // these are assets from the Gui package. Also no assets here yet.
    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let CursorSprite = asset<Image> PackageName "Cursor"