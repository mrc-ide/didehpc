@ECHO off
REM automatically generated
ECHO   host: %COMPUTERNAME%
ECHO   date: {{{date}}}
ECHO   didewin version: {{{didewin_version}}}
ECHO   context version: {{{context_version}}}
ECHO   dide task id: %CCP_TASKSYSTEMID%
set CONTEXT_TASK_ID={{{context_task_id}}}
set CONTEXT_WORKDRIVE={{{context_workdrive}}}
set CONTEXT_WORKDIR={{{context_workdir}}}
set CONTEXT_ROOT={{{context_root}}}
set CONTEXT_LOGFILE={{{context_logfile}}}
set CONTEXT_PROPAGATE_ERROR=TRUE

call setr{{{r_version}}}.bat

{{{#network_shares}}}
ECHO   mapping {{{drive}}} -^> {{{path}}}
net use {{{drive}}} {{{path}}} /y
{{{/network_shares}}}

{{{#parallel}}}
ECHO   This is a parallel job: will use %CPP_NUMCPUS%
set CONTEXT_CORES=%CCP_NUMCPUS%
{{{/parallel}}}

%CONTEXT_WORKDRIVE%
cd \%CONTEXT_WORKDIR%
ECHO   working directory: %CD%

ECHO   logfile: %CONTEXT_LOGFILE%
Rscript %CONTEXT_ROOT%\bin\context_runner %CONTEXT_ROOT% %CONTEXT_TASK_ID% > %CONTEXT_LOGFILE% 2>&1
echo Task completed successfully
