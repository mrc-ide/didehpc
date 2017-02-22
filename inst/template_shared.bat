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
set CONTEXT_ID={{{context_id}}}
set CONTEXT_PROPAGATE_ERROR=TRUE
set CONTEXT_BOOTSTRAP=TRUE

call setr{{{r_version}}}.bat

{{{#network_shares}}}
ECHO mapping {{{drive}}} -^> {{{path}}}
net use {{{drive}}} {{{path}}} /y
{{{/network_shares}}}

{{{#rtools}}}
ECHO Using Rtools at {{{rtools.path}}}
set PATH={{{rtools.path}}}\bin;{{{rtools.path}}}\gcc-{{{rtools.gcc}}}\bin;%PATH%
set BINPREF={{{rtools.binpref}}}/
{{{/rtools}}}

{{{#parallel}}}
ECHO This is a parallel job: will use %CPP_NUMCPUS%
set CONTEXT_CORES=%CCP_NUMCPUS%
{{{/parallel}}}

{{{#common_lib}}}
set R_LIBS={{{common_lib}}}
{{{/common_lib}}}

set REDIS_HOST={{{redis_host}}}
set REDIS_URL=redis://{{{redis_host}}}:6379

%CONTEXT_WORKDRIVE%
cd \%CONTEXT_WORKDIR%
ECHO working directory: %CD%
