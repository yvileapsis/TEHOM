namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module InteractableFacet =

    type Entity with
        member this.GetInteractableDisplayName world : String = this.Get (nameof this.InteractableDisplayName) world
        member this.SetInteractableDisplayName (value : String) world = this.Set (nameof this.InteractableDisplayName) value world
        member this.InteractableDisplayName = lens (nameof this.InteractableDisplayName) this this.GetInteractableDisplayName this.SetInteractableDisplayName

    type InteractableFacet () =
        inherit Facet (false, false, false)

        static member Properties = [
            define Entity.InteractableDisplayName "NO NAME!"
        ]

[<AutoOpen>]
module InventoryItemFacet =

    type Entity with
        member this.GetInventoryItemDisplayName world : String = this.Get (nameof this.InventoryItemDisplayName) world
        member this.SetInventoryItemDisplayName (value : String) world = this.Set (nameof this.InventoryItemDisplayName) value world
        member this.InventoryItemDisplayName = lens (nameof this.InventoryItemDisplayName) this this.GetInventoryItemDisplayName this.SetInventoryItemDisplayName

    type InventoryItemFacet () =
        inherit Facet (false, false, false)

        static member Properties = [
            define Entity.InventoryItemDisplayName "NO NAME!"
        ]