namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open FGL

(*
Issues (like say wounds) are new organs.


Systems:
* Energoskeletal - cardinal
* - kinetics, weight, pressure, energy, size
* - without this your organs are fine
* * Muscular - Fire - Aries
* * Joints - Water - Cancer
* * Adipose - Air - Libra
* * Skeletal - Earth - Capricorn
ATF is fuel.
Consumes energy on action.
Everything can serve as fuel, starting with glucose and ending with proteins.
Joints have limits and set up angles.
Skeleton sets distances and pressure.
Muscular system sets force.
Everything has weight.
No need for transportation, assumed possible always.
list bone joint bone joint musccle between organs


* Immunocirculatory - fixed
* - protection and resource management
* - without this your organs fail
* * Integumentary (skin) - Earth - Taurus
* * Cardiovascular - Fire - Leo
* * Lymphoid (lymph) - Water - Scorpio
* * Myeloid (bone marrow) - Air - Aquarius
There is venous and arterial blood, maybe more.
Organs take resources from one of the bloods and send outputs to the other.
Cancers take resources and grow.
Infections take resources and produce toxins. Maybe they should be looked at as reverse-organs.
Resources are represented similarly to pH levels, as log concentration of the element.
Skin outputs too outside.
Loss of blood is drain of all resources.
Water is just another resource.
Tissues determine throughput of resources. (?)


* Neuroendocrine - mutable
* - signals, what can and can't be reacted to, what can and can't be controlled
* - without this your organs are uncontrollable
* * Sensory - Air - Gemini
* * Endocrine - Earth - Virgo
* * Somatic - Fire - Sagittarius
* * Autonomous - Water - Pisces
Signals are counterracted by noise.
All signals are treated as the same abstract concept.
As signal travels from sensory organs it stops on the first organ on the line.
Organ processes the signal and issues new signals.
Complex interactions are managed by complex organs such as i.e. eyes.
Human eyes have two cones and two rods. This equates to list of four perceivers.
Each has minimal and maximal wavelength and noise ratio.
Exceeding maximum signal should stop eyes from processing information. (rods in the light, cones in the flashbang)


* Externalized Systems
* - Mostly consist of organs
* * Respiratory
* * Digestive
* * Urinary
* * Anything custom

Damage begins with skin


Pain meters take input from the systems and convert into some sort of score that determines type of pain and its intensity.
*)

module Energoskeletal =

    type Organ = {
        Distance : uint32 // bones
        Bearing : uint32 // joints
        Strength : uint32 // muscle
        Energy : uint32 // adipose
        Mass : uint32
    }

module Body =

(*

unlike with blood, energy doesn't have to be moved
depending on physical stance/action there are preferred organs
organs have energy requirements
no need to make this harder than ftl

floor/walls/ceiling/wings/air sacs are anchor points


*)


    type Organ =
        | Organ
        | Energoskeletal of Energoskeletal.Organ
    with
        static member empty = Organ

    type Tissue =
        | Energoskeletal

    type Body = {
        Organs : Graph<String, Organ, Tissue>
    }
    with
        static member empty = {
            Organs = Graph.empty
        }

        static member humanOrgans =

            let torso = Organ.Energoskeletal {
                Distance = 50u
                Bearing = 100u
                Strength = 100u
                Energy = 50u
                Mass = 55u
            }

            let arm = Organ.Energoskeletal {
                Distance = 100u
                Bearing = 100u
                Strength = 100u
                Energy = 10u
                Mass = 5u
            }

            let leg = Organ.Energoskeletal {
                Distance = 80u
                Bearing = 100u
                Strength = 100u
                Energy = 50u
                Mass = 20u
            }

            let head = Organ.Energoskeletal {
                Distance = 30u
                Bearing = 25u
                Strength = 25u
                Energy = 10u
                Mass = 5u
            }

            let joint = Tissue.Energoskeletal

            Graph.empty
            |> Vertices.add ("head", head)
            |> Vertices.add ("torso", torso)
            |> Vertices.add ("armLeft", arm)
            |> Vertices.add ("armRight", arm)
            |> Vertices.add ("legLeft", leg)
            |> Vertices.add ("legRight", torso)
            |> Undirected.Edges.add ("torso", "head", joint)
            |> Undirected.Edges.add ("torso", "armLeft", joint)
            |> Undirected.Edges.add ("torso", "armRight", joint)
            |> Undirected.Edges.add ("torso", "legLeft", joint)
            |> Undirected.Edges.add ("torso", "legRight", joint)

        static member human = {
            Body.empty with
                Organs = Body.humanOrgans
        }

        static member rat = Body.empty

        static member contains limb (body : Body) =
            Vertices.contains limb body.Organs


type Body = Body.Body