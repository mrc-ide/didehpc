REM automatically generated
REM   date: {{{date}}}
REM   didewin version: {{{didewin_version}}}
REM   context version: {{{context_version}}}
set CONTEXT_TASK_ID={{{context_task_id}}}
set CONTEXT_WORKDRIVE={{{context_workdrive}}}
set CONTEXT_WORKDIR={{{context_workdir}}}
set CONTEXT_ROOT={{{context_root}}}
set CONTEXT_LOGFILE={{{context_logfile}}}

call setr{{{r_version}}}.bat

{{{#network_shares}}}
net use {{{drive}}} {{{path}}}
{{{/network_shares}}}

%CONTEXT_WORKDRIVE%
cd %CONTEXT_WORKDIR%
cd
echo Logging to %CONTEXT_LOGFILE%
Rscript %CONTEXT_ROOT%\context_runner %CONTEXT_ROOT% %CONTEXT_TASK_ID% > %CONTEXT_LOGFILE% 2>&1
