namespace Tehom

open Nu


// Story is a list of scenes
//
module TehomAct =

    // world + time
    type Level = Level
    type Scene = Scene
    type Change = Change

    type Act = {
        Level: Level
        Scene: Scene
        Changes: List<Change>
    }

    type Story = List<Act>