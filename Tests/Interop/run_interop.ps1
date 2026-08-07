# =============================================================================
# Prueba de interoperabilidad A2A contra el SDK oficial (a2a-sdk)
# =============================================================================
# Levanta un servidor A2A de referencia -que serializa con los tipos protobuf
# del SDK, no con una interpretacion nuestra- y prueba las DOS direcciones:
#
#   1. Nuestro cliente (demo 072 en modo --client) contra el servidor de
#      referencia. Valida que sabemos LEER lo que emite un agente v1.0.
#   2. Un cliente conforme contra nuestro servidor (demo 072 en modo --serve).
#      Valida lo que EMITIMOS, parseandolo con json_format.ParseDict y
#      verificacion estricta de campos.
#
# Uso:
#   .\run_interop.ps1                 # crea el venv si falta y corre todo
#   .\run_interop.ps1 -SkipInstall    # asume el venv ya listo
#
# Requisitos: Python 3.10+ y el demo 072 compilado en Win64\Release.
# =============================================================================
param([switch]$SkipInstall)

$ErrorActionPreference = 'Stop'
$Root = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)   # ...\AiMaker
$Here = $PSScriptRoot
$Venv = Join-Path $Here ".venv"
$Py = Join-Path $Venv "Scripts\python.exe"
$Demo = Join-Path $Root "Demos\072-A2AFederation\Win64\Release\A2AFederationDemo.exe"
$PortRef = 8290    # servidor de referencia (Python)
$PortOurs = 8280   # nuestro servidor (Delphi)

if (-not (Test-Path $Demo)) {
  Write-Host "No encuentro el demo 072 compilado:" -ForegroundColor Red
  Write-Host "  $Demo"
  Write-Host "Compilalo primero: msbuild Demos\072-A2AFederation\A2AFederationDemo.dproj /p:Config=Release /p:Platform=Win64"
  exit 2
}

if (-not $SkipInstall) {
  if (-not (Test-Path $Py)) {
    Write-Host "Creando entorno virtual en $Venv ..."
    & python -m venv $Venv
  }
  Write-Host "Instalando a2a-sdk ..."
  & $Py -m pip install --quiet --upgrade pip
  & $Py -m pip install --quiet a2a-sdk
}

$ver = (& $Py -m pip show a2a-sdk | Select-String '^Version:').ToString()
Write-Host "SDK de referencia: $ver" -ForegroundColor Cyan

$fallos = 0

# --- Direccion 1: nuestro cliente -> servidor de referencia -------------------
Write-Host "`n=== 1. Nuestro cliente contra el servidor de referencia v1.0 ===" -ForegroundColor Cyan
$srv = Start-Process -FilePath $Py -ArgumentList "`"$Here\ref_server.py`"", $PortRef `
  -PassThru -WindowStyle Hidden
try {
  Start-Sleep -Seconds 3
  $out = & $Demo --client --url "http://localhost:$PortRef" "hola interop" 2>&1 | Out-String
  Write-Host $out
  # Debe traer el estado y el eco en mayusculas del servidor de referencia
  if ($out -match 'TASK_STATE_COMPLETED' -and $out -match 'HOLA INTEROP') {
    Write-Host "  [OK] leemos correctamente una respuesta v1.0" -ForegroundColor Green
  } else {
    Write-Host "  [FALLA] no supimos leer la respuesta del agente v1.0" -ForegroundColor Red
    $fallos++
  }
} finally {
  if ($srv -and -not $srv.HasExited) { Stop-Process -Id $srv.Id -Force }
}

# --- Direccion 2: cliente conforme -> nuestro servidor ------------------------
Write-Host "`n=== 2. Cliente v1.0 del SDK contra nuestro servidor ===" -ForegroundColor Cyan
$ours = Start-Process -FilePath $Demo -ArgumentList "--serve", "--port", $PortOurs `
  -PassThru -WindowStyle Hidden
try {
  Start-Sleep -Seconds 3
  $out = & $Py "$Here\ref_client.py" "http://localhost:$PortOurs" 2>&1 | Out-String
  Write-Host $out
  if ($out -match '\[FALLA\]') {
    Write-Host "  [FALLA] nuestro servidor no es conforme" -ForegroundColor Red
    $fallos++
  } else {
    Write-Host "  [OK] lo que emitimos parsea como v1.0" -ForegroundColor Green
  }
} finally {
  if ($ours -and -not $ours.HasExited) { Stop-Process -Id $ours.Id -Force }
}

Write-Host ""
if ($fallos -eq 0) {
  Write-Host "INTEROP OK: las dos direcciones conformes con la spec 1.0" -ForegroundColor Green
  exit 0
} else {
  Write-Host "INTEROP CON FALLOS: $fallos" -ForegroundColor Red
  exit 1
}
