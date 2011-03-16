@echo off

REM  -------------------------------------------------------
REM 	Author: Berlin Brown
REM	Date: 12/12/2004
REM
REM
REM 	ab.bat
REM
REM	Script to copy lisp code
REM	to Eclipse Workspace
REM  -------------------------------------------------------
REM
REM  Also: C:\ComputerTransfer\Eclipse\eclipse\workspace\Widget\lisp
REM
REM  -------------------------------------------------------

SET WORKSPACE="C:\Berlin\Downloads\ecl\eclipse\workspace\Widget\lisp"

copy d.lisp %WORKSPACE%
copy widget-toolkit.lisp %WORKSPACE%
copy widget-test.lisp %WORKSPACE%

