// Nu Slug Showcase.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SlugShowcase
open Nu

/// The Nu plugin used by standalone runtime and Gaia.
type SlugShowcasePlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofList
            [for scene in SlugDemoScene.all do
                (SlugDemoScene.label scene, fun world -> Game.SetSlugDemoScene scene world)]
