@echo off
cd /d "D:\Documentos\LibroIA\manuscript-v2\demos\cap22-rag-vectorial"
echo Cual es el plazo para devolver un producto?> input.txt
echo Cuanto cuesta la Laptop Pro X?>> input.txt
echo Hay envio gratis?>> input.txt
echo salir>> input.txt
Win64\Release\ChatbotDocumentacion.exe < input.txt > run_cap22.log 2>&1
echo RUN_EXIT_CODE=%ERRORLEVEL%
del input.txt 2>nul
