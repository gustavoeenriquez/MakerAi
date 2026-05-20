@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild "AnatomiaConversacion.dproj" /t:Build /p:Config=Release /p:Platform=Win64 /v:minimal
echo BUILD_EXIT_CODE=%ERRORLEVEL%
