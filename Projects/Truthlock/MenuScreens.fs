namespace Truthlock
open System
open System.Numerics
open Prime
open Nu
open Truthlock

type MenuModel =
    { MenuTime : int64 }

    static member initial =
        { MenuTime = 0L }

type MenuMessage =
    | MenuNil
    interface Message

type MenuCommand =
    | MenuNoop
    interface Command

[<RequireQualifiedAccess>]
module MenuContent =

    let private backdrop name =
        Content.panel name
            [Entity.Size == v3 640.0f 360.0f 0.0f
             Entity.Elevation == -10.0f
             Entity.Color == Style.Backdrop]
            []

    let private titleText text subtitle =
        [Content.text "Title"
            [Entity.Position == v3 0.0f 96.0f 0.0f
             Entity.Size == v3 560.0f 56.0f 0.0f
             Entity.Elevation == 10.0f
             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
             Entity.FontSizing == Some 34.0f
             Entity.TextColor == Style.Text
             Entity.Text == text]
         Content.text "Subtitle"
            [Entity.Position == v3 0.0f 54.0f 0.0f
             Entity.Size == v3 520.0f 28.0f 0.0f
             Entity.Elevation == 10.0f
             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
             Entity.FontSizing == Some 10.0f
             Entity.TextColor == Style.MutedText
             Entity.Text == subtitle]]

    let title =
        [Content.group Simulants.TitleGui.Name []
            ([backdrop "Backdrop"] @
             titleText "Truthlock" "A short Nu visual novel demo with one contradiction puzzle." @
             [Content.button Simulants.TitleStart.Name
                [Entity.Position == v3 0.0f -18.0f 0.0f
                 Entity.Size == v3 190.0f 34.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.Accent
                 Entity.Text == "Start Microcase"]
              Content.button Simulants.TitleCredits.Name
                [Entity.Position == v3 0.0f -62.0f 0.0f
                 Entity.Size == v3 190.0f 34.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.PanelLight
                 Entity.Text == "Credits"]
              Content.button Simulants.TitleExit.Name
                [Entity.Position == v3 0.0f -106.0f 0.0f
                 Entity.Size == v3 190.0f 34.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.PanelLight
                 Entity.Text == "Exit"]])]

    let ending =
        [Content.group Simulants.EndingGui.Name []
            ([backdrop "Backdrop"] @
             titleText "Truth Unlocked" "The contradiction is small, but the pattern is expandable." @
             [Content.text "Resolution"
                [Entity.Position == v3 0.0f 4.0f 0.0f
                 Entity.Size == v3 500.0f 56.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 10.0f
                 Entity.TextColor == Style.Text
                 Entity.Text == DemoContent.debateRound.SolvedText]
              Content.button Simulants.EndingReplay.Name
                [Entity.Position == v3 -112.0f -84.0f 0.0f
                 Entity.Size == v3 146.0f 32.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.Accent
                 Entity.Text == "Replay"]
              Content.button Simulants.EndingCredits.Name
                [Entity.Position == v3 52.0f -84.0f 0.0f
                 Entity.Size == v3 146.0f 32.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.PanelLight
                 Entity.Text == "Credits"]
              Content.button Simulants.EndingTitle.Name
                [Entity.Position == v3 216.0f -84.0f 0.0f
                 Entity.Size == v3 146.0f 32.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.PanelLight
                 Entity.Text == "Title"]])]

    let credits =
        [Content.group Simulants.CreditsGui.Name []
            ([backdrop "Backdrop"] @
             titleText "Truthlock" "Private fan-demo scaffold. Do not ship with ripped commercial assets." @
             [Content.text "CreditsBody"
                [Entity.Position == v3 0.0f -2.0f 0.0f
                 Entity.Size == v3 520.0f 86.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 9.0f
                 Entity.TextColor == Style.Text
                 Entity.Text == "Inspired by high-stakes VN mystery structure: dialogue, clues, trial logic, and room to grow into puzzle systems."]
              Content.button Simulants.CreditsBack.Name
                [Entity.Position == v3 0.0f -104.0f 0.0f
                 Entity.Size == v3 150.0f 32.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.Accent
                 Entity.Text == "Back"]])]

type TitleDispatcher () =
    inherit ScreenDispatcher<MenuModel, MenuMessage, MenuCommand> (MenuModel.initial)

    override this.Content (_, _) =
        MenuContent.title

type EndingDispatcher () =
    inherit ScreenDispatcher<MenuModel, MenuMessage, MenuCommand> (MenuModel.initial)

    override this.Content (_, _) =
        MenuContent.ending

type CreditsDispatcher () =
    inherit ScreenDispatcher<MenuModel, MenuMessage, MenuCommand> (MenuModel.initial)

    override this.Content (_, _) =
        MenuContent.credits
