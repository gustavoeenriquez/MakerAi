@echo off
cd /d "D:\Documentos\LibroIA\manuscript-v2\demos\cap29-providers-avanzado"
echo T> input.txt
echo.>> input.txt
Win64\Release\ProvidersCachingDemo.exe < input.txt > run_cap29.log 2>&1
echo RUN_EXIT_CODE=%ERRORLEVEL%
del input.txt 2>nul
