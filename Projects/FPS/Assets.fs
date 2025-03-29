﻿namespace MyGame
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let GuiSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; RepeatLimitOpt = None; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> PackageName "Gui" }
        let Crosshair = asset<Image> PackageName "Crosshair"
        let CrosshairPrompt = asset<Image> PackageName "CrosshairPrompt"


    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let DesertSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; RepeatLimitOpt = None; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> PackageName "Desert" }
        let SlashSound = asset<Sound> PackageName "Slash"
        let Slash2Sound = asset<Sound> PackageName "Slash2"
        let InjureSound = asset<Sound> PackageName "Injure"
        let HeartFull = asset<Image> PackageName "HeartFull"
        let HeartEmpty = asset<Image> PackageName "HeartEmpty"
        let JoanModel = asset<AnimatedModel> PackageName "Joan"
        let GreatSwordModel = asset<StaticModel> PackageName "GreatSword"
        let MakarovModel = asset<StaticModel> PackageName "makarov"
        let MakarovMagModel = asset<StaticModel> PackageName "makarov_std_cap_mag"
        let TrialGrounds2 = asset<StaticModel> PackageName "trial_grounds_2"
        let ShotSound = asset<Sound> PackageName "Shot"