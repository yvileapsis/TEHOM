namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open FGL

(*
Organs consume and produce resource same as money system.
There are 4 different systems.
Issues (like say wounds) are new organs.

Systems:
* Energomusculoskeletal - cardinal
* - kinetics, weight, pressure, energy, size
* - without this your organs are fine
* * Muscular - Fire - Aries
* * Joints - Water - Cancer
* * Adipose - Air - Libra
* * Skeletal - Earth - Capricorn

* Immunocirculatory - fixed
* - protection and resource management
* - without this your organs fail
* * Integumentary (skin) - Earth - Taurus
* * Cardiovascular - Fire - Leo
* * Lymphoid (lymph) - Water - Scorpio
* * Myeloid (bone marrow) - Air - Aquarius

* Neuroendocrine - mutable
* - signals, what can and can't be reacted to, what can and can't be controlled
* - without this your organs are uncontrollable
* * Sensory - Air - Gemini
* * Endocrine - Earth - Virgo
* * Somatic - Fire - Sagittarius
* * Autonomous - Water - Pisces

* Externalized Systems
* * Respiratory
* * Digestive
* * Urinary
* * Anything custom

Damage begins with skin
*)

module Body =

    type Limbs = Graph<String, Site, Relationship>

    type Body = {
        Mass: float
    }

type Body = Body.Body