@echo off
cd /d "D:\Documentos\LibroIA\manuscript-v2\demos\cap23-rag-grafos"
echo.> input.txt
Win64\Debug\CadenaSupministro.exe < input.txt > run_cap23.log 2>&1
echo RUN_EXIT_CODE=%ERRORLEVEL%
del input.txt 2>nul
