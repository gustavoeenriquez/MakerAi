@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild "E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker\Demos\Console\Demos02-ChatTools\07-AudioBridge\AudioBridge.dproj" /t:Build /p:Config=Debug /p:Platform=Win64 /v:minimal
echo BUILD_EXIT_CODE=%ERRORLEVEL%
msbuild "E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker\Demos\Console\OllamaAudioTest\OllamaAudioTest.dproj" /t:Build /p:Config=Debug /p:Platform=Win64 /v:minimal
echo OLLAMATEST_EXIT_CODE=%ERRORLEVEL%
