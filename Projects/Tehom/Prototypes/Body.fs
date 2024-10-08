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
l
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

* Externalized Systems
* - Mostly consist of organs
* * Respiratory
* * Digestive
* * Urinary
* * Anything custom

Damage begins with skin
*)

module Body =

    type Organ = Organ
    type Tissue = Tissue

    type Body = {
        Organs : Graph<String, Organ, Tissue>
    }

type Body = Body.Body