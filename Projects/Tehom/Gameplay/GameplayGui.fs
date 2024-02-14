namespace Tehom

open Nu
open Actor

module GameplayGui =

    let Background (gameplay: Gameplay) =

        Content.group Simulants.GameplayBackground.Name [] [
            Content.staticSprite "Background" [
                Entity.Size == Constants.Render.VirtualResolution.V3
                Entity.StaticImage == Assets.Default.Black
                Entity.Color == Color.Black
            ]
        ]

    let Gui (gameplay: Gameplay) =

        Content.group Simulants.GameplayGui.Name [] [// time

            Content.text Simulants.GameplayGametime.Name [
                Entity.Position == v3 384.0f 240.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text := string gameplay.GameTime
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSans12Font
                Entity.FontSizing == Some 10
            ]

            Content.text Simulants.GameplayTime.Name [
                Entity.Position == v3 240.0f 240.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text :=
                    $"Day: {TehomTime.getDay gameplay.Time} Time: %A{TehomTime.getHour gameplay.Time} {TehomTime.getMinute gameplay.Time}°"
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSans12Font
                Entity.FontSizing == Some 10
            ]

            richText (Simulants.GameplayTextBox.Name) [
                Entity.Size == v3 600.0f 100.0f 0.0f
                Entity.Position == v3 -36.0f 120.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                Entity.Text := """
1. **Testing a simple sentence.** 2. __Continuing testing a simple sentence.__ 3. *Continuing testing a simple sentence.* 4. ~~Finishing testing a simple sentence.~~

||'''||

|| 1. Testing a simple sentence. 2. Continuing testing a simple sentence. 3. Continuing testing a simple sentence. 4. Finishing testing a simple sentence.

|| 1. Testing a simple sentence. 2. Continuing testing a simple sentence. 3. Continuing testing a simple sentence. 4. Finishing testing a simple sentence. ||

1. Testing a simple sentence. 2. Continuing testing a simple sentence. 3. Continuing testing a simple sentence. 4. Finishing testing a simple sentence. ||

"""
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.MontSerratFont
                Entity.FontSizing == Some 10
            ]

            TextInput.textInput Simulants.GameplayTextInputBox.Name [
                Entity.Position == v3 -240.0f -144.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                Entity.Text == "Enter your string"
                Entity.TextInputChangedEvent =|> fun evt ->
                    let actorID = TehomID.ID (evt.Data)
                    InputString actorID
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSans12Font
                Entity.FontSizing == Some 10
            ]

            // Save
            Content.button Simulants.GameplaySave.Name [
                Entity.Position == v3 384.0f 192.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Text == "Save"
                Entity.ClickEvent => Save
            ]

            // Load
            Content.button Simulants.GameplayLoad.Name [
                Entity.Position == v3 384.0f 144.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Text == "Load"
                Entity.ClickEvent => Load
            ]

            // quit
            Content.button Simulants.GameplayQuit.Name [
                Entity.Position == v3 384.0f 96.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Text == "Quit"
                Entity.ClickEvent => StartQuitting
            ]

        ]