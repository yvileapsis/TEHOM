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

type CharacterDispatcher () =
    inherit EntityDispatcher<Character, Message, Command> (true, false, false, false, Character.empty)

    override this.Definitions (_, _) =
        []

    override this.Render (_, _, _, _) =
        ()

[<AutoOpen>]
module CharacterEntity =
    let character (character : Character) =
        Content.entity<CharacterDispatcher> character.ID [ Entity.Character == character ]