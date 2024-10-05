namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open FGL

(*
Distance : float

Bones : bool
Muscles : bool

Veins : bool
Arteries : bool

Sensory : bool
Motor : bool


Connective tissue
Bones
Cartilage
Blood
Adipose

Epithelial

Muscle

Neural

Damage begins with skin
*)

module Body =

    type Limbs = Graph<String, Site, Relationship>

    type Body = {
        Mass: float
    }

type Body = Body.Body