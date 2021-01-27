@echo off
rem
rem create hdaplay.exe, using jwasm and HXDEV tools:
rem loadpe.bin: simple PE loader
rem pestub: exchange DOS stub of PE binary
rem patchpe: change PE to PX, set stack and heap values
rem
jwasm -nologo -pe -Sg -Fl hdaplay.asm
pestub -n -q hdaplay.exe loadpe.bin
patchpe -x -s:8192 -h:0 hdaplay.exe
