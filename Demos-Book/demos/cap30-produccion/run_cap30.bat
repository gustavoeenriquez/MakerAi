@echo off
cd /d "D:\Documentos\LibroIA\manuscript-v2\demos\cap30-produccion"
echo.> input.txt
Win64\Debug\PatronesProd.exe < input.txt > run_cap30.log 2>&1
echo RUN_EXIT_CODE=%ERRORLEVEL%
del input.txt 2>nul
