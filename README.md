# didewin

> DIDE Windows Cluster Support

Nothing in here will stay the same.

# A meta queue

A queue that generates queues...

In some cases it will be useful to create a pool of tasks that will be worked on as part of a pool to avoid overloading the queuing system.  I'll add an argument: `metaqueue=TRUE` then which will indicate that the task should not be submitted to the cluster at all, their id into a directory `<root>/metaqueue` (can just be a blank file).  Job status remains PENDING, though it might be useful to distinguish betweenn PENDING and QUEUED.

Then a second set of workers are submitted.  They'll launch and look for a task to run by being in the metaqueue.  These tasks will have a timeout perhaps for how long they should wait with no tasks.

The worker copies existing files out of the `metaqueue` directory into a subdirectory there (being the worker); if that task succeeds it may run the job.  Run the job by spawning a second R instance.

The actual metaqueue might be easier to write in Python, though I don't know.

This would also be able to run locally which would be pretty cool.  If doing it in R put in another package.  But it could be a pretty simple little Python script, especially as then we could have ~8 cores worth :)

The important thing is that there's nothing blocking based on the design _except_ for having to overload the enqueue functions.
