ECHO this is an worker

{{=<% %>=}}
set RRQ_WORKER_ID={{{worker_id}}}
<%={{ }}=%>

set REDIS_HOST={{{redis_host}}}
set RRQ_WORKER_KEY_ALIVE={{{rrq_key_alive}}}
set CONTEXT_LOGFILE={{{context_root}}}\{{{worker_log_path}}}\%RRQ_WORKER_ID%
ECHO logfile: %CONTEXT_LOGFILE%

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript "{{{context_root}}}\bin\rrq_worker" --context-root "%CONTEXT_ROOT%" --context-id {{{context_id}}} --redis-host %REDIS_HOST% --key-alive "%RRQ_WORKER_KEY_ALIVE%" --worker-name %RRQ_WORKER_ID% --timeout {{{worker_timeout}}} --log-path {{{log_path}}} > "%CONTEXT_LOGFILE%" 2>&1

@echo Quitting
