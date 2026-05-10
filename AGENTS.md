# Agent Workflow

This workspace contains the Nu engine plus the active game project at `Projects/Sudoku`. Treat `Projects/Sudoku` as the default implementation target unless the user explicitly asks for engine work under `Nu/`.

## Architecture Grounding

- Use `Architecture.md` as the local Nu architecture source of truth, especially before architectural changes.
- Nu centers on `World`, a small mutable handle over an otherwise persistent `WorldState`. Do not describe normal Nu simulation state as mutable except for the `World` handle, internal optimizations, or explicitly imperative state.
- Preserve the simulant hierarchy: `Game -> Screen -> Group -> Entity -> Facets`. Groups are the scene composition layer between screens and entities.
- Sudoku is MMCC-oriented. Keep gameplay state in typed models, messages, commands, and declarative content.
- Prefer changing gameplay behavior through model/message/content flow. Do not manually create, destroy, or mutate children owned by MMCC content from unrelated code.

## Project Conventions

- Keep stable simulant handles in `Projects/Sudoku/Simulants.fs`.
- Keep typed game events in `Projects/Sudoku/Events.fs`.
- Keep asset constants and package names in `Projects/Sudoku/Assets.fs`, with package definitions in `Projects/Sudoku/AssetGraph.nuag`.
- Keep editor/runtime plugin behavior, initial packages, edit modes, and ImGui editor integrations in `Projects/Sudoku/SudokuPlugin.fs`.
- Gate runtime-only effects with `world.Unaccompanied` so Gaia/editor-accompanied workflows are not disrupted.
- Preserve Gaia editor compatibility and live-edit behavior when changing gameplay or plugin code.

## Editing Workflow

- Before editing, inspect the relevant Sudoku module and nearby Nu patterns instead of guessing.
- Keep changes scoped to `Projects/Sudoku` unless the user explicitly asks for broader repository or engine changes.
- For F# edits, respect the compile order in `Projects/Sudoku/Sudoku.fsproj`. If adding a source file, insert it after the files it depends on and before files that depend on it.
- Avoid unrelated refactors, generated asset churn, and reverting user changes.
- If existing user changes are present, work with them. Do not reset, checkout, or otherwise discard them unless the user explicitly requests it.

## Validation Policy

- Do not run local `dotnet build`, `dotnet test`, compile checks, or Nu.Pipe unless the user explicitly requests them.
- Validation is expected to happen through Gaia editor.
- Do not provide detailed Gaia testing instructions by default.
- When local validation is skipped because of this policy, state that plainly in the final response.
