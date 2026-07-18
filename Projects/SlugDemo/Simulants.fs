namespace SlugDemo
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    let screen scene = Game / SlugDemoScene.name scene
    let sceneGroup scene = screen scene / "Scene"
    let previousButton scene = sceneGroup scene / "Previous"
    let currentDemo scene = sceneGroup scene / "CurrentDemo"
    let nextButton scene = sceneGroup scene / "Next"
