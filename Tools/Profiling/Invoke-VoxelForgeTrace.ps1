param(
    [ValidateSet("Debug", "Release")]
    [string] $Configuration = "Release",

    [ValidateSet("WorldGeneration", "Gameplay", "Title", "Default")]
    [string] $StartupMode = "WorldGeneration",

    [int] $ReadyTimeoutSeconds = 180,
    [int] $TraceSeconds = 60,
    [int] $ExerciseSeconds = 45,
    [int] $JumpIntervalMilliseconds = 900,
    [int] $JumpHoldMilliseconds = 120,

    [ValidateSet("cpu-sampling", "gc-verbose")]
    [string] $TraceProfile = "cpu-sampling",

    [ValidateSet("NetTrace", "Speedscope", "Chromium")]
    [string] $TraceFormat = "NetTrace",

    [string] $OutputRoot = "",
    [string] $ReadyLogPattern = "VoxelForge entering generated world",

    [switch] $SkipBuild,
    [switch] $NoExercise,
    [switch] $InstallDotnetTrace,
    [switch] $KeepAppOpen,
    [switch] $DryRun
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Get-RepoRoot {
    $scriptDir = Split-Path -Parent $PSCommandPath
    (Resolve-Path (Join-Path $scriptDir "..\..")).Path
}

function ConvertTo-CommandLineArgument {
    param([Parameter(Mandatory = $true)][string] $Argument)
    if ($Argument -notmatch '[\s"]') { return $Argument }
    '"' + $Argument.Replace('"', '\"') + '"'
}

function Join-CommandLine {
    param([Parameter(Mandatory = $true)][string[]] $Arguments)
    ($Arguments | ForEach-Object { ConvertTo-CommandLineArgument $_ }) -join " "
}

function Get-StartupArguments {
    param([Parameter(Mandatory = $true)][string] $Mode)
    switch ($Mode) {
        "WorldGeneration" { @("--profile-start-world-generation") }
        "Gameplay" { @("--profile-start-gameplay") }
        "Title" { @("--profile-start-title") }
        "Default" { @() }
    }
}

function Get-TraceExtension {
    param([Parameter(Mandatory = $true)][string] $Format)
    switch ($Format) {
        "NetTrace" { "nettrace" }
        "Speedscope" { "speedscope.json" }
        "Chromium" { "chromium.json" }
    }
}

function Resolve-DotnetTrace {
    param(
        [Parameter(Mandatory = $true)][string] $RepoRoot,
        [switch] $Install
    )

    $command = Get-Command "dotnet-trace" -ErrorAction SilentlyContinue
    if ($null -ne $command) {
        return @{
            File = $command.Source
            Prefix = @()
        }
    }

    $toolDir = Join-Path $RepoRoot ".codex-tmp\dotnet-tools"
    $toolExe = Join-Path $toolDir "dotnet-trace.exe"
    if (Test-Path $toolExe) {
        return @{
            File = $toolExe
            Prefix = @()
        }
    }

    if ($Install) {
        New-Item -ItemType Directory -Force -Path $toolDir | Out-Null
        & dotnet tool update dotnet-trace --tool-path $toolDir | Out-Host
        if ($LASTEXITCODE -ne 0) { throw "dotnet tool update dotnet-trace failed." }
        if (-not (Test-Path $toolExe)) { throw "dotnet-trace did not appear at '$toolExe' after installation." }
        return @{
            File = $toolExe
            Prefix = @()
        }
    }

    $manifestPath = Join-Path $RepoRoot ".config\dotnet-tools.json"
    if ((Test-Path $manifestPath) -and ((Get-Content -Raw -Path $manifestPath) -match '"dotnet-trace"')) {
        return @{
            File = "dotnet"
            Prefix = @("tool", "run", "dotnet-trace", "--")
        }
    }

    throw "dotnet-trace was not found. Install it globally, restore a local tool manifest that contains dotnet-trace, or rerun this script with -InstallDotnetTrace."
}

function Wait-ForMainWindow {
    param(
        [Parameter(Mandatory = $true)][System.Diagnostics.Process] $Process,
        [int] $TimeoutSeconds = 30
    )

    $deadline = [DateTime]::UtcNow.AddSeconds($TimeoutSeconds)
    while ([DateTime]::UtcNow -lt $deadline) {
        if ($Process.HasExited) { throw "VoxelForge exited before creating a window." }
        $Process.Refresh()
        if ($Process.MainWindowHandle -ne [IntPtr]::Zero) { return $true }
        Start-Sleep -Milliseconds 100
    }
    return $false
}

function Wait-ForLogPattern {
    param(
        [Parameter(Mandatory = $true)][string] $LogPath,
        [Parameter(Mandatory = $true)][string] $ReadyMarkerPath,
        [Parameter(Mandatory = $true)][string] $Pattern,
        [Parameter(Mandatory = $true)][System.Diagnostics.Process] $Process,
        [int] $TimeoutSeconds = 180
    )

    if ([string]::IsNullOrWhiteSpace($Pattern) -or $TimeoutSeconds -le 0) { return $true }
    $deadline = [DateTime]::UtcNow.AddSeconds($TimeoutSeconds)
    while ([DateTime]::UtcNow -lt $deadline) {
        if ($Process.HasExited) { throw "VoxelForge exited while waiting for gameplay readiness." }
        if (Test-Path $ReadyMarkerPath) { return $true }
        if (Test-Path $LogPath) {
            $text = Get-Content -Raw -Path $LogPath -ErrorAction SilentlyContinue
            if ($text -match $Pattern) { return $true }
        }
        Start-Sleep -Milliseconds 250
    }
    return $false
}

function Add-NativeInputType {
    if ("VoxelForgeTrace.NativeInput" -as [type]) { return }
    Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;

namespace VoxelForgeTrace
{
    public static class NativeInput
    {
        [DllImport("user32.dll")]
        public static extern bool SetForegroundWindow(IntPtr hWnd);

        [DllImport("user32.dll")]
        public static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);

        [DllImport("user32.dll")]
        public static extern void keybd_event(byte bVk, byte bScan, uint dwFlags, UIntPtr dwExtraInfo);

        public const int SW_SHOW = 5;
        public const uint KEYEVENTF_KEYUP = 0x0002;

        public static void KeyDown(byte virtualKey)
        {
            keybd_event(virtualKey, 0, 0, UIntPtr.Zero);
        }

        public static void KeyUp(byte virtualKey)
        {
            keybd_event(virtualKey, 0, KEYEVENTF_KEYUP, UIntPtr.Zero);
        }
    }
}
"@
}

function Focus-ProcessWindow {
    param([Parameter(Mandatory = $true)][System.Diagnostics.Process] $Process)
    $Process.Refresh()
    if ($Process.MainWindowHandle -eq [IntPtr]::Zero) { return $false }
    [VoxelForgeTrace.NativeInput]::ShowWindow($Process.MainWindowHandle, [VoxelForgeTrace.NativeInput]::SW_SHOW) | Out-Null
    [VoxelForgeTrace.NativeInput]::SetForegroundWindow($Process.MainWindowHandle) | Out-Null
    Start-Sleep -Milliseconds 100
    return $true
}

function Invoke-GameplayExercise {
    param(
        [Parameter(Mandatory = $true)][System.Diagnostics.Process] $Process,
        [int] $Seconds,
        [int] $JumpIntervalMilliseconds,
        [int] $JumpHoldMilliseconds
    )

    if ($Seconds -le 0) { return }
    Add-NativeInputType

    $vkW = [byte]0x57
    $vkSpace = [byte]0x20
    $endTime = [DateTime]::UtcNow.AddSeconds($Seconds)
    $nextJump = [DateTime]::UtcNow.AddMilliseconds(600)
    $holdingForward = $false

    try {
        Focus-ProcessWindow $Process | Out-Null
        [VoxelForgeTrace.NativeInput]::KeyDown($vkW)
        $holdingForward = $true

        while ([DateTime]::UtcNow -lt $endTime -and -not $Process.HasExited) {
            if ([DateTime]::UtcNow -ge $nextJump) {
                Focus-ProcessWindow $Process | Out-Null
                [VoxelForgeTrace.NativeInput]::KeyDown($vkSpace)
                Start-Sleep -Milliseconds $JumpHoldMilliseconds
                [VoxelForgeTrace.NativeInput]::KeyUp($vkSpace)
                $nextJump = [DateTime]::UtcNow.AddMilliseconds($JumpIntervalMilliseconds)
            }
            Start-Sleep -Milliseconds 25
        }
    }
    finally {
        [VoxelForgeTrace.NativeInput]::KeyUp($vkSpace)
        if ($holdingForward) { [VoxelForgeTrace.NativeInput]::KeyUp($vkW) }
    }
}

function Start-ProcessWithRedirect {
    param(
        [Parameter(Mandatory = $true)][string] $FilePath,
        [Parameter(Mandatory = $true)][string[]] $Arguments,
        [Parameter(Mandatory = $true)][string] $WorkingDirectory,
        [Parameter(Mandatory = $true)][string] $StdOutPath,
        [Parameter(Mandatory = $true)][string] $StdErrPath
    )

    Start-Process `
        -FilePath $FilePath `
        -ArgumentList (Join-CommandLine $Arguments) `
        -WorkingDirectory $WorkingDirectory `
        -RedirectStandardOutput $StdOutPath `
        -RedirectStandardError $StdErrPath `
        -PassThru
}

$repoRoot = Get-RepoRoot
$projectPath = Join-Path $repoRoot "Projects\VoxelForge\VoxelForge.fsproj"
$outputRootResolved =
    if ([string]::IsNullOrWhiteSpace($OutputRoot)) {
        Join-Path $repoRoot ".codex-tmp\traces\voxel-forge"
    }
    else {
        if ([System.IO.Path]::IsPathRooted($OutputRoot)) { $OutputRoot }
        else { Join-Path $repoRoot $OutputRoot }
    }
$runStamp = Get-Date -Format "yyyyMMdd-HHmmss"
$runDir = Join-Path $outputRootResolved $runStamp
$appOut = Join-Path $runDir "voxel-forge.stdout.log"
$appErr = Join-Path $runDir "voxel-forge.stderr.log"
$readyMarker = Join-Path $runDir "voxel-forge.ready.json"
$traceOut = Join-Path $runDir "dotnet-trace.stdout.log"
$traceErr = Join-Path $runDir "dotnet-trace.stderr.log"
$metadataPath = Join-Path $runDir "run-metadata.json"
$tracePath = Join-Path $runDir ("voxel-forge-" + $runStamp + "." + (Get-TraceExtension $TraceFormat))
$targetDir = Join-Path $repoRoot ("Projects\VoxelForge\bin\" + $Configuration + "\net10.0")
$appExe = Join-Path $targetDir "VoxelForge.exe"
$appDll = Join-Path $targetDir "VoxelForge.dll"
$startupArgs = Get-StartupArguments $StartupMode

New-Item -ItemType Directory -Force -Path $runDir | Out-Null

if ($DryRun) {
    Write-Host "Repo: $repoRoot"
    Write-Host "Project: $projectPath"
    Write-Host "Run dir: $runDir"
    Write-Host "Startup args: $(Join-CommandLine $startupArgs)"
    Write-Host "Ready marker: $readyMarker"
    Write-Host "Trace output: $tracePath"
    return
}

if (-not $SkipBuild) {
    Write-Host "Building VoxelForge ($Configuration)..."
    & dotnet build $projectPath -c $Configuration | Tee-Object -FilePath (Join-Path $runDir "dotnet-build.log")
    if ($LASTEXITCODE -ne 0) { throw "VoxelForge build failed." }
}

if (Test-Path $appExe) {
    $appFile = $appExe
    $appArgs = $startupArgs
}
elseif (Test-Path $appDll) {
    $appFile = "dotnet"
    $appArgs = @($appDll) + $startupArgs
}
else {
    throw "VoxelForge output was not found under '$targetDir'. Build the project first or omit -SkipBuild."
}

$dotnetTrace = Resolve-DotnetTrace -RepoRoot $repoRoot -Install:$InstallDotnetTrace
$duration = [TimeSpan]::FromSeconds($TraceSeconds).ToString("c")
$traceArgs =
    @($dotnetTrace.Prefix) +
    @(
        "collect",
        "--process-id", "0",
        "--profile", $TraceProfile,
        "--format", $TraceFormat,
        "--output", $tracePath,
        "--duration", $duration
    )

$readyMarkerPrevious = $env:VOXELFORGE_PROFILE_READY_FILE
$env:VOXELFORGE_PROFILE_READY_FILE = $readyMarker
try {
    Write-Host "Launching VoxelForge..."
    $appProcess = Start-ProcessWithRedirect `
        -FilePath $appFile `
        -Arguments $appArgs `
        -WorkingDirectory $targetDir `
        -StdOutPath $appOut `
        -StdErrPath $appErr
}
finally {
    if ($null -eq $readyMarkerPrevious) { Remove-Item Env:\VOXELFORGE_PROFILE_READY_FILE -ErrorAction SilentlyContinue }
    else { $env:VOXELFORGE_PROFILE_READY_FILE = $readyMarkerPrevious }
}

try {
    Wait-ForMainWindow -Process $appProcess -TimeoutSeconds 30 | Out-Null
    $ready = Wait-ForLogPattern -LogPath $appOut -ReadyMarkerPath $readyMarker -Pattern $ReadyLogPattern -Process $appProcess -TimeoutSeconds $ReadyTimeoutSeconds
    if (-not $ready) {
        Write-Warning "Ready marker / log pattern was not observed before timeout; collecting trace anyway."
    }

    $traceArgs[2 + $dotnetTrace.Prefix.Count] = [string]$appProcess.Id
    $metadata = [ordered]@{
        repoRoot = $repoRoot
        configuration = $Configuration
        startupMode = $StartupMode
        appPid = $appProcess.Id
        appFile = $appFile
        appArguments = $appArgs
        traceProfile = $TraceProfile
        traceFormat = $TraceFormat
        traceSeconds = $TraceSeconds
        exerciseSeconds = if ($NoExercise) { 0 } else { $ExerciseSeconds }
        jumpIntervalMilliseconds = $JumpIntervalMilliseconds
        jumpHoldMilliseconds = $JumpHoldMilliseconds
        readyLogPattern = $ReadyLogPattern
        readyObserved = $ready
        readyMarker = $readyMarker
        tracePath = $tracePath
        appStdout = $appOut
        appStderr = $appErr
        dotnetTraceStdout = $traceOut
        dotnetTraceStderr = $traceErr
        startedUtc = [DateTime]::UtcNow.ToString("o")
    }
    $metadata | ConvertTo-Json -Depth 4 | Set-Content -Path $metadataPath

    Write-Host "Collecting trace from PID $($appProcess.Id)..."
    $traceProcess = Start-ProcessWithRedirect `
        -FilePath $dotnetTrace.File `
        -Arguments $traceArgs `
        -WorkingDirectory $repoRoot `
        -StdOutPath $traceOut `
        -StdErrPath $traceErr

    if (-not $NoExercise) {
        Invoke-GameplayExercise `
            -Process $appProcess `
            -Seconds ([Math]::Min($ExerciseSeconds, $TraceSeconds)) `
            -JumpIntervalMilliseconds $JumpIntervalMilliseconds `
            -JumpHoldMilliseconds $JumpHoldMilliseconds
    }

    $traceProcess.WaitForExit()
    if ($traceProcess.ExitCode -ne 0) {
        throw "dotnet-trace failed with exit code $($traceProcess.ExitCode). See '$traceErr'."
    }
    Write-Host "Trace complete: $tracePath"
}
finally {
    if (-not $KeepAppOpen -and -not $appProcess.HasExited) {
        Stop-Process -Id $appProcess.Id -Force -ErrorAction SilentlyContinue
    }
}
