@echo off
cd /d "D:\Documentos\LibroIA\manuscript-v2\demos\cap33-ppm-skills"
echo.> input.txt
Win64\Release\SkillsDemo.exe < input.txt > run_cap33.log 2>&1
echo RUN_EXIT_CODE=%ERRORLEVEL%
del input.txt 2>nul
