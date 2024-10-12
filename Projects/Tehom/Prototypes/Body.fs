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

module Body =

    type Organ = Organ
    type Tissue = Tissue

    type Body = {
        Organs : Graph<String, Organ, Tissue>
    }

type Body = Body.Body