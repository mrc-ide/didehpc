ECHO this is a worker

{{=<% %>=}}
set CONTEXT_WORKER_ID={{{worker_id}}}
<%={{ }}=%>

set CONTEXT_LOGFILE={{{context_root}}}\workers\%CONTEXT_WORKER_ID%
ECHO logfile: %CONTEXT_LOGFILE%

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript "{{{context_root}}}\bin\worker_runner" "%CONTEXT_ROOT%" {{{context_id}}} > "%CONTEXT_LOGFILE%" 2>&1

@echo Quitting
