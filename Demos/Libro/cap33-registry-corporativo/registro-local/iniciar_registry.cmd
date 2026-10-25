@echo off
rem Registry PPM corporativo de Cafe La Ladera, en esta maquina (capitulo 33).
rem Requiere PostgreSQL con la base ppm_ladera (ver LEEME.md) y PPMServer.exe compilado
rem desde https://github.com/gustavoeenriquez/ppm (carpeta server). Las claves NO van aqui:
rem se toman del entorno (PPM_DB_PASS y PPM_SECRET_KEY) antes de ejecutar este script.
set PPM_DB_HOST=127.0.0.1
set PPM_DB_PORT=5432
set PPM_DB_NAME=ppm_ladera
set PPM_DB_USER=postgres
set PPM_PORT=8766
set PPM_STORAGE_PATH=%~dp0storage
set PPM_MAX_UPLOAD_MB=50
set PPM_VERSION=1.0.0-ladera
if "%PPM_DB_PASS%"=="" echo Falta PPM_DB_PASS en el entorno && exit /b 1
if "%PPM_SECRET_KEY%"=="" echo Falta PPM_SECRET_KEY en el entorno && exit /b 1
if not exist "%PPM_STORAGE_PATH%" mkdir "%PPM_STORAGE_PATH%"
PPMServer.exe
