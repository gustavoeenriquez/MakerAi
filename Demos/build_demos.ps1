# ===========================================================================
# build_demos.ps1 - Compila todos los demos de MakerAI Suite (FPC Port)
# Equivalente Windows de build_demos.sh
# ===========================================================================
# Uso:
#   .\build_demos.ps1                        # usa el FPC del PATH o el de Lazarus
#   .\build_demos.ps1 -Fpc C:\fpc\fpc.exe    # FPC especifico
#   .\build_demos.ps1 -Clean                 # borra bin\ y lib\
# ===========================================================================
param(
  [string]$Fpc = '',
  [switch]$Clean
)

$ErrorActionPreference = 'Stop'
$Root   = Split-Path -Parent $MyInvocation.MyCommand.Path
$Source = Join-Path (Split-Path -Parent $Root) 'Source'

if ($Clean) {
  foreach ($d in @('bin', 'lib')) {
    $p = Join-Path $Root $d
    if (Test-Path $p) { Remove-Item $p -Recurse -Force }
  }
  Write-Host 'Limpiado bin\ y lib\. Ejecuta sin -Clean para recompilar.'
  exit 0
}

# --- localizar el compilador ---
if (-not $Fpc) {
  $cmd = Get-Command fpc -ErrorAction SilentlyContinue
  if ($cmd) {
    $Fpc = $cmd.Source
  } else {
    $Fpc = Get-ChildItem 'C:\lazarus\fpc' -Recurse -Filter 'fpc.exe' -ErrorAction SilentlyContinue |
           Select-Object -First 1 -ExpandProperty FullName
  }
}
if (-not $Fpc -or -not (Test-Path $Fpc)) {
  Write-Host 'No se encontro fpc.exe. Usa -Fpc <ruta>.' -ForegroundColor Red
  exit 1
}

# --- rutas de unidades (las mismas que documenta CLAUDE.md) ---
$UnitDirs = @('Core','Chat','Design','Tools','Agents','RAG','MCPClient','MCPServer','Utils')
$Fu = $UnitDirs | ForEach-Object { "-Fu$Source\$_" }
$Fu += "-Fu$Root"          # uDemoHelper
$Fi = "-Fi$Source\Core"    # uMakerAi.Version.inc

$BinDir = Join-Path $Root 'bin'
$LibDir = Join-Path $Root 'lib'
New-Item -ItemType Directory -Force $BinDir, $LibDir | Out-Null

$demos = Get-ChildItem $Root -File -Filter '*.pas' |
         Where-Object { $_.Name -ne 'uDemoHelper.pas' } | Sort-Object Name

Write-Host "Compilando $($demos.Count) demos"
Write-Host "  FPC: $Fpc"
Write-Host ('=' * 63)

$ok = 0; $fail = 0; $failList = @()
$log = Join-Path $env:TEMP 'makerai_build.txt'
$i = 0

foreach ($d in $demos) {
  $i++
  Write-Host ("  [{0,2}] {1,-38} " -f $i, $d.Name) -NoNewline
  & $Fpc "-FE$BinDir" "-FU$LibDir" $Fi @Fu $d.FullName > $log 2>&1
  if ($LASTEXITCODE -eq 0) {
    Write-Host 'OK' -ForegroundColor Green
    $ok++
  } else {
    Write-Host 'FAIL' -ForegroundColor Red
    $fail++; $failList += $d.Name
    Select-String -Path $log -Pattern 'Fatal:|Error:' |
      Select-Object -First 3 | ForEach-Object { Write-Host "       $($_.Line)" -ForegroundColor DarkYellow }
  }
}

Write-Host ('=' * 63)
Write-Host "  OK: $ok  |  FAIL: $fail  |  Total: $($demos.Count)"
if ($fail -gt 0) {
  Write-Host ''
  Write-Host '  Fallaron:'
  $failList | ForEach-Object { Write-Host "    $_" -ForegroundColor Red }
  exit 1
}
Write-Host "  Ejecutables en: $BinDir" -ForegroundColor Green
