@echo off

REM Register .cmdrc.bat

reg add "HKEY_CURRENT_USER\Software\Microsoft\Command Processor" /v AutoRun /d %~dp0.cmdrc.bat
