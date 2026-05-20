@echo off
cd /d "D:\Documentos\LibroIA\manuscript-v2\demos\cap26-agentes"
echo.> input.txt
Win64\Release\AgentesDemo.exe < input.txt > run_cap26.log 2>&1
echo RUN_EXIT_CODE=%ERRORLEVEL%
del input.txt 2>nul
