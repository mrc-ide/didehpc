@echo off
ECHO generated on host: {{{hostname}}}
ECHO generated on date: {{{date}}}
ECHO didehpc version: {{{didehpc_version}}}
ECHO conan version: {{{conan_version}}}
ECHO running on: %COMPUTERNAME%

call setr64_{{{r_version}}}.bat

{{{#network_shares}}}
ECHO mapping {{{drive}}} -^> {{{path}}}
net use {{{drive}}} {{{path}}} /y
{{{/network_shares}}}

ECHO Using Rtools at {{{rtools.rtools_root}}}
set PATH={{{rtools.gcc_path}}};{{{rtools.make_path}}};%PATH%
set BINPREF={{{rtools.binpref}}}/

{{{#conan_root}}}
set CONAN_PATH_BOOTSTRAP={{{conan_root}}}\bootstrap
set CONAN_PATH_CACHE={{{conan_root}}}\cache
{{{/conan_root}}}

{{=<% %>=}}
set CONAN_ID={{{conan_id}}}
<%={{ }}=%>

set CONAN_LOGFILE={{{context_root}}}\conan\log\%CONAN_ID%
ECHO logfile: %CONAN_LOGFILE%

ECHO on
Rscript "{{{context_root}}}\conan\bin\%CONAN_ID%" "{{{r_libs_user}}}" > "%CONAN_LOGFILE%" 2>&1

@ECHO off
%SystemDrive%
set ErrorCode=%ERRORLEVEL%

{{{#network_shares}}}
ECHO Removing mapping {{{drive}}} -^> {{{path}}}
net use {{{drive}}} /delete /y
{{{/network_shares}}}

set ERRORLEVEL=%ErrorCode%

if %ERRORLEVEL% neq 0 (
  ECHO Error running conan
  EXIT /b %ERRORLEVEL%
)

@ECHO Quitting
