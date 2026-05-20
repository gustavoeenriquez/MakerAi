@echo off
cd /d "D:\Documentos\LibroIA\manuscript-v2\demos\cap31-ppm-gestor"
echo.> input.txt
Win64\Release\PPMDemo.exe < input.txt > run_cap31.log 2>&1
echo RUN_EXIT_CODE=%ERRORLEVEL%
del input.txt 2>nul
