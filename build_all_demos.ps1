$rsvars = "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
$base    = "E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker\Demos\054-AutoAgents"
$logfile = "E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker\build_all.log"

$demos = @(
    "01-HelloLLM\D01_HelloLLM.dproj",
    "02-SingleTool\D02_SingleTool.dproj",
    "03-MultiTools\D03_MultiTools.dproj",
    "04-SequentialPipeline\D04_SequentialPipeline.dproj",
    "05-ParallelResearch\D05_ParallelResearch.dproj",
    "06-ConditionalRouter\D06_ConditionalRouter.dproj",
    "07-ResearchAgent\D07_ResearchAgent.dproj",
    "08-ValidateAndRetry\D08_ValidateAndRetry.dproj",
    "09-MultiAgentDebate\D09_MultiAgentDebate.dproj",
    "10-HumanInTheLoop\D10_HumanInTheLoop.dproj"
)

$results = @()

foreach ($demo in $demos) {
    $dproj = Join-Path $base $demo
    $name  = [System.IO.Path]::GetFileNameWithoutExtension($demo)
    Write-Host "Building $name ..." -NoNewline

    $bat = "@echo off`r`ncall `"$rsvars`"`r`nmsbuild `"$dproj`" /t:Build /p:Config=Release /p:Platform=Win64 /v:minimal`r`necho BUILD_EXIT_CODE=%ERRORLEVEL%"
    $tmpBat = "$env:TEMP\build_tmp.bat"
    [System.IO.File]::WriteAllText($tmpBat, $bat)

    $output = cmd /c "$tmpBat" 2>&1
    $exitLine = ($output | Where-Object { $_ -match 'BUILD_EXIT_CODE=' }) -replace 'BUILD_EXIT_CODE=', ''
    $exitCode = [int]($exitLine.Trim())

    $errors = $output | Where-Object { $_ -match '\[Error\]|\[error\]| error ' } | Select-Object -First 5

    if ($exitCode -eq 0) {
        Write-Host " OK" -ForegroundColor Green
        $results += [PSCustomObject]@{ Demo=$name; Status="OK"; Errors="" }
    } else {
        Write-Host " FAILED" -ForegroundColor Red
        foreach ($e in $errors) { Write-Host "  $e" -ForegroundColor Yellow }
        $results += [PSCustomObject]@{ Demo=$name; Status="FAILED"; Errors=($errors -join " | ") }
    }
    # Save full log
    ($output -join "`n") | Out-File $logfile -Append
    "--- END $name ---`n" | Out-File $logfile -Append
}

Write-Host ""
Write-Host "=== BUILD SUMMARY ===" -ForegroundColor Cyan
$results | Format-Table -AutoSize
