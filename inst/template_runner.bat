ECHO this is a single task

{{=<% %>=}}
set CONTEXT_TASK_ID={{{task_id}}}
<%={{ }}=%>

set CONTEXT_LOGFILE={{{context_root}}}\{{{log_path}}}\%CONTEXT_TASK_ID%
ECHO logfile: %CONTEXT_LOGFILE%

@REM The quoting here is necessary for paths with spaces.
ECHO on
Rscript "{{{context_root}}}\bin\task_run" "%CONTEXT_ROOT%" %CONTEXT_TASK_ID% > "%CONTEXT_LOGFILE%" 2>&1

@echo Quitting
