namespace Tehom

open Nu

module TehomAct =

    // world + time + quests
    type Location = Level of float * string

    type Condition =
         | Condition

    // world + time + quest changes
    type Effect = Effect
    type Effects = Effect list

    // Scene is a constraint on what player should be able to do
    type Opener = {
        Name: string
        Text: string
        // Limitations:
        // Actors/stars
    }

    // TODO: I should design these blocks actually
    type Scene =
        | Opener of Opener
        | Action
        | Container
        | Combat
        | Death


    // random thought - scene end is basically a writing carriage

    (*
        What is a new scene?
        Text, images, gameplay windows
        They have actions player can take, they expose?? already present entities
        They make them important, highlight them. Even if they're not present they can still be interacted with
        But this is immersive sim part

        New scenes are selected as a reaction to player's actions.

        Consider a normal experience:

        "You are in a well-lit room with many chairs and a *surgical table*, a *cat* is on the *table*.
        There also is a *drawer*."

        Therefore scene is sort of conditioned by world having these items accessible and could have
        additional variation based on what is present and what's missing.

        Once you do some action with an entity, or, say, something happens by itself, a new scene appears
        i.e. you say "look at the table" or "open table" or "kick table", all those put it into focus and
        depending on the action and the object a new scene should appear. This is a hard task and should fall
        onto condition system. So, it should get access to the last action.

        Now at this point a new scene appears which is a container name, container contents, drawn dynamically in
        a grid/invisible grid/or some other form of visual interface. I could have different types of inventories tbf.
        What I want is for at least some of them to feel structured or auto-structuring even. Not every single one has
        to be like that! Dirty post-apo shelve can be piles of garbage.

        Whenever you move stuff within the container (typically) nothing happens, or well it's consequence-less.
        The state of items is possibly reflected in the drawn container object, but can't back-propagate to the
        scene.

        When you move stuff to or from a container (including across containers (this just implies two
        consecutive events)) this is basically a shorthand for player action "take x from container", so it checks
        whether you have hands, item can be taken out, etc, etc or the action is forbidden and there is some brief
        flash or whatever to show the action is impossible.

        Scene can be exhausted and in the case of container it's said container being emptied or filled to the brim.
        That's the goal of a container.

        Gameplay scenes are self-contained as much as possible, if you took out and inserted something a million times
        what gets recorded is that nothing happened. Result of a scene is always changes in the world. A list of
        actions or effects onto the game world in the wide sense.

        After container scene is exhausted you can leave the act (which is figured out by you fulfilling exit scene
        condition or failing continuation of presence condition on header act, same thing) or you can continue
        messing with other highlighted entities.

        The highlighted entities should be named actors, as they're the main participants in the act.

        In general you are allowed everything until some scene disallows some action. I.e. if a combat scene started
        you can no longer exit. Of course those compound and several different scenes can lock you in more.
        Death could also be a scene, it definitely should be, actually. It locks you the most. And by messing with
        scene end you can easily make yourself deathless.

        Minimal plan:
        1. I need to make a very simple key is in the cat scene.
        2. It has to be properly displayed.
        3. It must be properly selected from the world.
        4. Should highlight cat.

    *)

    type Deck =
        | Conditioned of List<Condition> * Deck
        | List of Deck list
        | Scene of Scene


    type Act = {
        Location: Location
        Scenes: (Scene * Effects) list
        ActEnd: int option
    }

    // this is what gets stored in world model
    type Story = Act list