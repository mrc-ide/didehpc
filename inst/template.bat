@echo off
REM automatically generated
ECHO generated on host: {{{hostname}}}
ECHO generated on date: {{{date}}}
ECHO didewin version: {{{didewin_version}}}
ECHO context version: {{{context_version}}}
ECHO running on: %COMPUTERNAME%
set CONTEXT_WORKDRIVE={{{context_workdrive}}}
set CONTEXT_WORKDIR={{{context_workdir}}}
set CONTEXT_ROOT={{{context_root}}}
{{{#context_task_id}}}
ECHO this is a single task
set CONTEXT_TARGET={{{context_task_id}}}
set CONTEXT_LOGFILE={{{context_root}}}\logs\{{{context_task_id}}}
set CONTEXT_RUNNER={{{context_root}}}\bin\context_runner
{{{/context_task_id}}}
{{{^context_task_id}}}
ECHO this is a worker
set CONTEXT_TARGET={{{context_id}}}
set CONTEXT_LOGFILE={{{context_root}}}\workers\{{{worker_id}}}
set CONTEXT_RUNNER={{{context_root}}}\bin\worker_runner
{{{/context_task_id}}}

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
Rscript "%CONTEXT_RUNNER%" "%CONTEXT_ROOT%" %CONTEXT_TARGET% > "%CONTEXT_LOGFILE%" 2>&1

@echo Quitting
