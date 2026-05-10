namespace Tehom

open System
open Prime
open Nu

[<AutoOpen>]
module CharacterExtensions =
    type Entity with
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()
        member this.SetCharacterWith updater world =
            let character = this.GetCharacter world
            let character = updater character
            this.SetCharacter character world
        member this.GetCharacterWith getter world =
            let character = this.GetCharacter world
            let value = getter character
            value

type CharacterDispatcher () =
    inherit EntityDispatcher<Character, Message, Command> (true, false, false, false, Character.empty)

    override this.Definitions (_, _) =
        []

    override this.Render (_, _, _, _) =
        ()

[<AutoOpen>]
module CharacterEntity =
    let character (character : Character) =
        Content.composite<CharacterDispatcher> character.ID [
            Entity.Character == character
        ] [

        ]