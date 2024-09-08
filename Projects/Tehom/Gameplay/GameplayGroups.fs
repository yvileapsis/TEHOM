namespace Tehom

open Nu
open Act

module GameplayGroups =

    let background = Content.group "GameplayBackground" [] [
        Content.staticSprite "Background" [
            Entity.Size == Constants.Render.VirtualResolution.V3
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.Black
        ]
    ]

    let sceneName (model: Gameplay) = Content.group "GameplaySceneName" [] [

        let position = v3 0.0f 156.0f 0.0f

        let topName =
            match Story.getActName model.CurrentAct model.Story with
            | None -> ""
            | Some name -> name

        Content.text "Gameplay" [
            Entity.Size == v3 400.0f 20.0f 0.0f
            Entity.Position == position
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Text := topName
            Entity.TextColor == Color.FloralWhite
            Entity.Font == Assets.Gui.MontSerratFont
            Entity.FontSizing == Some 10
        ]
    ]

    let actionInput (gameplay : Gameplay) = Content.group "GameplayActionInput" [] [
        Content.text "GameplayActionInputBox" [
            Entity.Position == v3 -160.0f -96.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
            Entity.Text == "Enter your string"
            //Entity.TextInputChangedEvent =|> fun evt ->
            //    let actorID = TehomID.ID (evt.Data)
            //    InputString actorID
            Entity.TextColor == Color.FloralWhite
            Entity.Font == Assets.Gui.ClearSansFont
            Entity.FontSizing == Some 10
        ]
    ]

    let scenes (gameplay: Gameplay) = Content.group "GameplayScenes" [] [// time
        let position = v3 0.0f 0.0f 0.0f

        Content.association "GameplayScenesAssociation" [
            Entity.Size == v3 400.0f 240.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Position == position
            Entity.Layout == Flow (FlowDownward, FlowUnlimited)
        ] [
            let act = Story.getAct gameplay.CurrentAct gameplay.Story

            match act with

            | Some act ->
                let scenes = act.Scenes


                for i, scene in List.indexed scenes do
                    richText ("GameplayScenesAssociationText" + $"{i}") [
                        Entity.Size == v3 400.0f 100.0f 0.0f
//                            Entity.Position == position + v3 0f (float32 i * 30.0f) 0f
//                            Entity.Elevation == 10.0f
                        Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                        Entity.Text := Scene.getText (scene |> fst) act.Stage act.Player
                        Entity.TextColor == Color.FloralWhite
                        Entity.Font == Assets.Gui.MontSerratFont
                        Entity.FontSizing == Some 10
                    ]
            | None -> ()
            ]
        ]


        (*
            Content.text Simulants.GameplayTime.Name [
                Entity.Position == v3 160.0f 160.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text :=
                    $"Day: {TehomTime.getDay gameplay.Time} Time: %A{TehomTime.getHour gameplay.Time} {TehomTime.getMinute gameplay.Time}°"
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSans12Font
                Entity.FontSizing == Some 10

            Content.text (Simulants.GameplayScenes.Name + "/Text1") [
                Entity.Position == v3 256.0f 160.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text := string gameplay.GameTime
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSans12Font
                Entity.FontSizing == Some 10
            ]

            ]

            """
1. **Testing a simple sentence.** 2. __Continuing testing a simple sentence.__ 3. *Continuing testing a simple sentence.* 4. ~~Finishing testing a simple sentence.~~

||'''||

|| 1. Testing a simple sentence. 2. Continuing testing a simple sentence. 3. Continuing testing a simple sentence. 4. Finishing testing a simple sentence.

|| 1. Testing a simple sentence. 2. Continuing testing a simple sentence. 3. Continuing testing a simple sentence. 4. Finishing testing a simple sentence. ||

1. Testing a simple sentence. 2. Continuing testing a simple sentence. 3. Continuing testing a simple sentence. 4. Finishing testing a simple sentence. ||

"""

            *)