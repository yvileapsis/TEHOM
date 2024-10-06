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

Distance : float

Systems:
* Musculoskeletal
* * Skeletal
* * Joints
* * Muscular

* Circulatory
* * Cardiovascular
* * Lymphatic

* Nervous
* * Somatic
* * Sensory
* * Autonomous

* Immune
* * Myeloid (bone marrow)
* * Lymphoid (lymph)
* * Integumentary (skin)

* Respiratory

* Endocrine

not doing these ones
* Digestive
* Urinary
* Reproductive

Tissues:
* Connective tissue : Earth
* * Bones : Earth
* * Cartilage : Water
* * Blood : Air
* * Adipose : Fire

* Epithelial : Lymph

* Muscle : Gall

* Neural : Plasma

Damage begins with skin
*)

module Body =

    type Limbs = Graph<String, Site, Relationship>

    type Body = {
        Mass: float
    }

type Body = Body.Body