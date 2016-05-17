@echo off
REM automatically generated
ECHO generated on host: %COMPUTERNAME%
ECHO generated on date: {{{date}}}
ECHO didewin version: {{{didewin_version}}}
ECHO context version: {{{context_version}}}
ECHO dide task id: %CCP_TASKSYSTEMID%
ECHO running on: %COMPUTERNAME%
set CONTEXT_TASK_ID={{{context_task_id}}}
set CONTEXT_WORKDRIVE={{{context_workdrive}}}
set CONTEXT_WORKDIR={{{context_workdir}}}
set CONTEXT_ROOT={{{context_root}}}
set CONTEXT_LOGFILE={{{context_logfile}}}
set CONTEXT_PROPAGATE_ERROR=TRUE

call setr{{{r_version}}}.bat

{{{#network_shares}}}
ECHO mapping {{{drive}}} -^> {{{path}}}
net use {{{drive}}} {{{path}}} /y
{{{/network_shares}}}

{{{#parallel}}}
ECHO This is a parallel job: will use %CPP_NUMCPUS%
set CONTEXT_CORES=%CCP_NUMCPUS%
{{{/parallel}}}

{{{#rtools}}}
ECHO Using Rtools at {{{rtools.path}}}
set PATH={{{rtools.path}}}\bin;{{{rtools.path}}}\gcc-{{{rtools.gcc}}}\bin;%PATH%
{{{/rtools}}}

%CONTEXT_WORKDRIVE%
cd \%CONTEXT_WORKDIR%
ECHO working directory: %CD%

ECHO logfile: %CONTEXT_LOGFILE%

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript "%CONTEXT_ROOT%\bin\context_runner" "%CONTEXT_ROOT%" %CONTEXT_TASK_ID% > "%CONTEXT_LOGFILE%" 2>&1

@echo Quitting
