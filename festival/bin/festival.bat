@echo off

rem batch file to start Festival
rem $Id: festival.bat,v 1.1 2004/04/26 21:14:27 emarsi Exp $

rem This batch file assumes that it is in a directory 'bin',
rem th eparent of wich contains Festival's library files in 'lib', 
rem and a festival.exe in 'src\main' 

rem add path to cygwin1.dll, sh.exe, mbrola.exe, etc.
set PATH=.;..\lib\etc;%PATH%

rem Set HOME, where festival looks .festivalrc 
rem and writes .festival_history
rem On recent windows
if "%HOME%" == "" set HOME=%HOMEDRIVE%%HOMEPATH%
rem otherwise, guess it's the C: drive
if "%HOME%" == "" set HOME=C:

rem Set TMPDIR, which is used by Festival's mk_temp_filename function
rem We need this to write temp Mbrola files
if "%TMPDIR%" == "" set TMPDIR=%TMP%
if "%TMPDIR%" == "" set TMPDIR=%TEMP%
if "%TMPDIR%" == "" set TMPDIR=C:

rem call festival, note the forward slash in the lib dir!
..\src\main\festival --libdir ../lib

rem if the direct call above fails, try to run it through a shell:
rem sh -c "../src/main/festival.exe --libdir=../lib"

rem FIXME: is there a way to close the Command window?