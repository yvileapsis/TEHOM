namespace Truthlock
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Style =

    let Backdrop = Color (0.06f, 0.06f, 0.08f, 1.0f)
    let BackdropAlt = Color (0.10f, 0.11f, 0.14f, 1.0f)
    let Panel = Color (0.15f, 0.16f, 0.20f, 0.94f)
    let PanelLight = Color (0.24f, 0.25f, 0.30f, 0.94f)
    let Accent = Color (0.86f, 0.10f, 0.22f, 1.0f)
    let AccentAlt = Color (0.10f, 0.62f, 0.76f, 1.0f)
    let Warning = Color (0.92f, 0.55f, 0.10f, 1.0f)
    let Text = Color.GhostWhite
    let MutedText = Color (0.72f, 0.76f, 0.82f, 1.0f)
