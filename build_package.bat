@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild "E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker\Source\Packages\MakerAI.dproj" /t:Build /p:Config=Release /p:Platform=Win64 /v:minimal
echo PACKAGE_EXIT_CODE=%ERRORLEVEL%
