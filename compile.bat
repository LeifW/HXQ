@echo off

rem -- compile an XQuery file
rem    usage:  compile xquery-file object-file

xquery -c %1
if "%2"=="" goto Exit
set FILE="%2"
goto End
:Exit
set FILE="a.exe"
:End
ghc -O2 -v0 -funfolding-use-threshold=16 --make Temp.hs -o %FILE%
