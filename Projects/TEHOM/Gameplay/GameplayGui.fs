namespace Tehom

open Nu
open Actor

module GameplayGui =

    let Gui (gameplay: Gameplay) =
        Content.group Simulants.GameplayGui.Name [] [// time

        Content.text Simulants.GameplayTime.Name [
            Entity.Position == v3 0.0f 232.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Text := string gameplay.Time
        ]

        Content.text Simulants.GameplayTextBox.Name [
            Entity.Position == v3 0.0f 132.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Text := gameplay.Display
        ]

        TextInput.textInput Simulants.GameplayTextInputBox.Name [
            Entity.Position == v3 0.0f 0.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
            Entity.Text == "Enter your string"
            Entity.TextInputChangedEvent =|> fun evt ->
                let actorID = TehomID.ID (evt.Data)
                DoAction actorID
        ]

        // quit
        Content.button Simulants.GameplayQuit.Name [
            Entity.Position == v3 336.0f -216.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Text == "Quit"
            Entity.ClickEvent => StartQuitting
        ]

        // Save
        Content.button Simulants.GameplaySave.Name [
            Entity.Position == v3 136.0f -116.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Text == "Save"
            Entity.ClickEvent => Save
        ]

        // Load
        Content.button Simulants.GameplayLoad.Name [
            Entity.Position == v3 -136.0f -116.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Text == "Load"
            Entity.ClickEvent => Load
        ]
    ]