echo "This is a single task"

{{=<% %>=}}
export CONTEXT_TASK_ID={{{task_id}}}
<%={{ }}=%>

export CONTEXT_LOGFILE="{{{context_root}}}/{{{log_path}}}/${CONTEXT_TASK_ID}"
echo "logfile: $CONTEXT_LOGFILE"

# The quoting here is necessary for paths with spaces.
Rscript "{{{context_root}}}/bin/task_run" "$CONTEXT_ROOT" $CONTEXT_TASK_ID > "$CONTEXT_LOGFILE" 2>&1

echo Quitting
