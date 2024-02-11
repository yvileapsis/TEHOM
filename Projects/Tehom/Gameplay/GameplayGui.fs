namespace Tehom

open Nu
open Actor

module GameplayGui =

    let Gui (gameplay: Gameplay) =

        Content.group Simulants.GameplayGui.Name [] [// time

            Content.staticSprite "Background" [
                Entity.Size == Constants.Render.VirtualResolution.V3
                Entity.StaticImage == Assets.Default.Black
                Entity.Color == Color.Black
            ]

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


            // TODO: Make something sensible instead of this crutch
            for i, string in gameplay.Display.Split [|'\n'; '\r'|] |> Array.indexed do
                let y = 216.0f - float32 i * 12.0f
                let x = if y < -120.0f then 40.0f else -240.0f
                let y = if y < -120.0f then 558.0f - float32 i * 12.0f else y

                Content.text (Simulants.GameplayTextBox.Name + $"{i}") [
                    Entity.Position == v3 x y 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                    Entity.Text := string
                    Entity.TextColor == Color.FloralWhite
                    Entity.Font == Assets.Gui.MontSerratFont
                    Entity.FontSizing == Some 10
               ]

            richText (Simulants.GameplayTextBox.Name + "!" + "test") [
                Entity.Size == v3 600.0f 100.0f 0.0f
                Entity.Position == v3 -36.0f 120.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                Entity.Text := """
| Left

Right |

| Center |

**Bold**

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