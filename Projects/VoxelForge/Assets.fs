﻿namespace MyGame
open System
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"
        let GuiSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; RepeatLimitOpt = None; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> PackageName "Gui" }

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

    [<RequireQualifiedAccess>]
    module Voxels =
        let PackageName = "Voxels"
        let Computer = asset<Image> PackageName "computer_u"
        let Minecraft = asset<Image> PackageName "minecraft"
        let Cars = asset<Image> PackageName "cars"
        let Wood = asset<Image> PackageName "wood_block"
        let Grass = asset<Image> PackageName "grass_block"