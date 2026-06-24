namespace Psychocrawl
open Nu
open Psychocrawl

type PsychocrawlPlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofList
            [("Gameplay", fun world -> Game.SetPsychocrawl Psychocrawl.initial world)]

    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]
