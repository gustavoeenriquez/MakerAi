@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild "D:\Documentos\LibroIA\manuscript-v2\demos\cap30-produccion\PatronesProd.dproj" /t:Rebuild /p:Config=Debug /p:Platform=Win64 /v:minimal
echo BUILD_EXIT_CODE=%ERRORLEVEL%
