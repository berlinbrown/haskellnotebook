@echo off

REM ---------------------------------------------
REM
REM  Berlin Brown
REM  build.bat
REM
REM ---------------------------------------------

REM  /// Set the make path accordingly

SET MAKE_PATH=C:\Dev-Cpp\bin

%MAKE_PATH%\make.exe -f "Makefile.win" all

copy WidgetToolkit.dll "..\WidgetTest\"


REM
REM --- End of Script
REM
