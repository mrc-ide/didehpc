ECHO this is an worker

set RRQ_CONFIG={{cluster_name}}
set RRQ_WORKER_ID={{{{rrq_worker_id}}}}

set CONTEXT_LOGFILE={{context_root}}\{{rrq_worker_log_path}}\%RRQ_WORKER_ID%
ECHO logfile: %CONTEXT_LOGFILE%

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript "{{context_root}}\bin\rrq_worker" --config didehpc --worker-id %RRQ_WORKER_ID% %CONTEXT_ID% > "%CONTEXT_LOGFILE%" 2>&1

@ECHO off
%SystemDrive%
set ErrorCode=%ERRORLEVEL%

{{network_shares_delete}}

@ECHO off
if %ERRORLEVEL% neq 0 (
  ECHO Error running task
  EXIT /b %ERRORLEVEL%
)

@echo Quitting
