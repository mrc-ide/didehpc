@echo off
REM automatically generated
ECHO generated on host: {{hostname}}
ECHO generated on date: {{date}}
ECHO didehpc version: {{didehpc_version}}
ECHO context version: {{context_version}}
ECHO running on: %COMPUTERNAME%
set CONTEXT_WORKDRIVE={{context_workdrive}}
set CONTEXT_WORKDIR={{context_workdir}}
set CONTEXT_ROOT={{context_root}}
set CONTEXT_ID={{context_id}}
set R_LIBS_USER={{r_libs_user}}

call setr64_{{r_version}}.bat

REM If Java is wanted, then call setJava64.
REM If called with blank, it adds default JRE.

IF '{{use_java}}'=='TRUE' (
  call setJava64.bat {{java_home}}
)

{{network_shares_create}}

ECHO Using Rtools at {{rtools$rtools_root}}
set PATH={{rtools$gcc_path}};{{rtools$make_path}};%PATH%
set BINPREF={{rtools$binpref}}/

{{parallel}}

set REDIS_HOST={{redis_host}}
set REDIS_URL=redis://{{redis_host}}:6379

%CONTEXT_WORKDRIVE%
cd \%CONTEXT_WORKDIR%
ECHO working directory: %CD%
