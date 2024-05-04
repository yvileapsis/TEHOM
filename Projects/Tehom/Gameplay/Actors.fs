namespace Tehom
open System
open Nu

module Actors =

    type Limb = Limb
    type Tissue = Tissue

    type Actors = {
        Limbs : Graph<Limb, Tissue>
    }

    type Relations = Relations

    type Area = {
        Actors : Graph<Actors, Relations>
    }

    type Pathway = Pathway

    type Stage = {
        Name : string
        Areas : Graph<Area, Pathway>
    }

    type Portal = Portal

    type Abyss = {
        Stages : Graph<Stage, Portal>
    }