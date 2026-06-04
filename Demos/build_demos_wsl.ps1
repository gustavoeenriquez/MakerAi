param(
  [Parameter(Position = 0, ValueFromRemainingArguments = $true)]
  [string[]]$BuildArgs,

  [string]$Distro = ''
)

$ErrorActionPreference = 'Stop'

function ConvertTo-BashSingleQuoted {
  param([string]$Value)
  return "'" + $Value.Replace("'", "'""'""'") + "'"
}

$wslArgs = @()
if ($Distro -ne '') {
  $wslArgs += @('-d', $Distro)
}

$demoDirOutput = & wsl @wslArgs -e wslpath -a -u $PSScriptRoot
$demoDirExitCode = $LASTEXITCODE
$demoDir = ($demoDirOutput | Select-Object -First 1).Trim()
if ($demoDirExitCode -ne 0 -or $demoDir -eq '') {
  throw 'WSL path conversion failed. Check that WSL is installed and the repository path is mounted.'
}

$quotedArgs = @()
foreach ($arg in $BuildArgs) {
  $quotedArgs += ConvertTo-BashSingleQuoted $arg
}

$argText = ''
if ($quotedArgs.Count -gt 0) {
  $argText = ' ' + ($quotedArgs -join ' ')
}

$command = @"
set -euo pipefail
cd $(ConvertTo-BashSingleQuoted $demoDir)
tmp_script="./.build_demos_wsl_`$`$.sh"
sed 's/\r$//' ./build_demos.sh > "`$tmp_script"
chmod +x "`$tmp_script"
trap 'rm -f "`$tmp_script"' EXIT
bash "`$tmp_script"$argText
"@

& wsl @wslArgs -e bash -lc $command
exit $LASTEXITCODE
