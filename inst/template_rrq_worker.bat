ECHO this is an worker

set RRQ_CONFIG={{cluster_name}}
set RRQ_WORKER_ID={{{{rrq_worker_id}}}}
set RRQ_KEY_ALIVE={{{{rrq_key_alive}}}}

set CONTEXT_LOGFILE={{context_root}}\{{rrq_worker_log_path}}\%RRQ_WORKER_ID%
ECHO logfile: %CONTEXT_LOGFILE%

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript "{{context_root}}\bin\rrq_worker" "%CONTEXT_ROOT%" %CONTEXT_ID% %RRQ_CONFIG% %RRQ_WORKER_ID% %RRQ_KEY_ALIVE% > "%CONTEXT_LOGFILE%" 2>&1

{{network_shares_delete}}

@ECHO off
if %ERRORLEVEL% neq 0 (
  ECHO Error running task
  EXIT /b %ERRORLEVEL%
)

@echo Quitting
