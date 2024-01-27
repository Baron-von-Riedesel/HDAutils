@echo off
rem
rem create hdastat.exe, using jwasm and HXDEV tools:
rem loadpe.bin: simple PE loader
rem pestub: exchange DOS stub of PE binary
rem patchpe: change PE to PX, set stack and heap values
rem
jwasm -nologo -pe -Sg -Fl=build\hdastat.lst -Fo=build\hdastat.exe hdastat.asm
pestub -n -q build\hdastat.exe loadpe.bin
patchpe -x -s:8192 -h:0 build\hdastat.exe
