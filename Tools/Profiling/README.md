# VoxelForge Trace Harness

`Invoke-VoxelForgeTrace.ps1` launches VoxelForge, skips into generated-world startup, waits for gameplay readiness, collects a `dotnet-trace` trace, and drives a repeatable movement pattern by holding `W` and pulsing `Space`.

The script passes `VOXELFORGE_PROFILE_READY_FILE` to the child process. VoxelForge writes that marker after the gameplay chunk entities are installed, so profiling starts after world generation / initial chunk realization rather than during startup.

Typical run from the repository root:

```powershell
.\Tools\Profiling\Invoke-VoxelForgeTrace.ps1 -Configuration Release -TraceSeconds 90 -ExerciseSeconds 75
```

If `dotnet-trace` is not installed globally, install it into `.codex-tmp\dotnet-tools` for this repo:

```powershell
.\Tools\Profiling\Invoke-VoxelForgeTrace.ps1 -InstallDotnetTrace
```

Outputs are written under `.codex-tmp\traces\voxel-forge\<timestamp>\`:

- `voxel-forge-*.nettrace` or another selected trace format.
- `voxel-forge.stdout.log` / `voxel-forge.stderr.log`.
- `voxel-forge.ready.json` when gameplay is ready.
- `dotnet-trace.stdout.log` / `dotnet-trace.stderr.log`.
- `run-metadata.json` with PID, configuration, trace settings, and artifact paths.

Useful options:

- `-TraceFormat Speedscope` writes a Speedscope JSON trace.
- `-TraceProfile gc-verbose` collects GC-focused events instead of CPU sampling.
- `-SkipBuild` reuses the existing build output.
- `-NoExercise` collects a stationary gameplay trace.
- `-StartupMode Gameplay` skips generated-world generation and starts the fallback gameplay path.
- `-DryRun` prints paths and startup arguments without launching anything.
