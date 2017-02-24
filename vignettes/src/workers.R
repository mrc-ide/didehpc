## ---
## title: "R and the DIDE cluster, workers"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output:
##   rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{Workers}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo = FALSE,results = "hide"
source("common.R")

## # Running heaps of jobs without annoying your colleagues

## If you have thousands and thousands of jobs to submit at once you
## may not want to flood the cluster with them all at once.  Each job
## submission is relatively slow (the HPC tools that the web interface
## has to use are relatively slow).  The actual queue that the cluster
## uses doesn't seem to like processing tens of thousands of job, and
## can slow down.  And if you take up the whole cluster someone may
## come and knock on your office and complain at you.  At the same
## time, batching your jobs up into little bits and manually sending
## them off is a pain and work better done by a computer.

## An alternative is to submit a set of "workers" to the cluster, and
## then submit jobs to them.  This is done with the
## [`rrq`](https://github.com/richfitz/rrq) package, along with a
## [`redis`](http://redis.io) server running on the cluster.

## To get started you will need to install the rrq package locally.
## This will also install [`redux`](https://github.com/richfitz/redux)
## which can be a bit of a pain to install on some platforms (talk to
## Rich if you have trouble).

## ```r
## drat::add("dide-tools")
## install.packages("rrq")
## ```
##
## or
##
## ```r
## source("https://dide-tools.github.io/didehpc/install#extras")
## ```

## The context id is used for communication, so if there is a chance
## that someone else may come up with the same context id as you (same
## packages and names of source files) you need to create a small bit
## of unique information and pack that into the context.  This will
## change the id and ensure that the tasks will not collide.  Your
## username or project name make a good piece of data to use here.  Be
## sure not to use a value that will be randomly generated each time
## though.
unique_value <- "rfitzjoh"

##+ echo = FALSE
unique_value <- ids::random_id()

context::context_log_start()
root <- "context_workers"
ctx <- context::context_save(root,
                             packages = "ape",
                             sources = "mysources.R",
                             unique_value = unique_value)

## There are two ways we can proceed from here; the first - "workers"
## - is very similar to the non-worker workflow and is described
## first.  The second - "rrq" - is a bit more involved and is
## described second.

## ## Workers

## Then configure and create the queue:
config <- didehpc::didehpc_config(use_workers = TRUE)
obj <- didehpc::queue_didehpc(ctx, config = config)

## All passing `use_workers` here will do is arrange to install `rrq`,
## `redux` and their dependencies on the cluster, plus enable a couple
## of methods in the queue objects.  Aside from that it's essentially
## the same queue as before.

## The big difference is that submitting jobs (either with `enqueue`
## or via one of the `queuer` bulk submission functions) will no
## longer submit jobs to the DIDE queue, but to an internal Redis
## queue.

## You must submit actual workers (you can do this before or after
## submitting jobs but I'll do it first here).  The argument is the
## number of workers to submit.
workers <- obj$submit_workers(5)
workers

##+ echo = FALSE
Sys.sleep(0.5)

## All workers get names in the form `<adjective>_<animal>_<integer>`
## so that you can remember which workers you set off.  They will turn
## off after 10 minutes of inactivity by default (you can tweak this
## with the `worker_timeout` argument to `didehpc_config` or by
## sending a `TIMEOUT_SET` message).

## Submitting jobs works as before, but should hopefully be a little
## faster:
t <- obj$enqueue(make_tree(5))
t$wait(5)

## One advantage over the usual queuing approach here is that you will
## not wait for anyone elses jobs to complete once you have reserved
## your workers.

## You can see what your workers have been up to with the
## `workers_log_tail` command:
obj$workers$workers_log_tail(n = Inf)

## The `time` column may be tweaked into something a bit more
## practical soon.

## As before, logging works on a per-task basis:
t$log()

## Find out how long your workers will persist for:
id <- obj$workers$send_message("TIMEOUT_GET")
obj$workers$get_responses(id, timeout = 10)

## Other than that, hopefully everything else continues as normal.  We
## can submit a bunch of jobs and run them using `queuer::qlapply`:
sizes <- 3:8
grp <- queuer::qlapply(sizes, make_tree, obj)

## Task status:
grp$status()

## Collect the results:
res <- grp$wait(5)
res

## While workers will turn off automatically, it's polite to turn them
## off as soon as you're done using `obj$stop_workers()`

## Alternatively, after submitting a bunch of jobs you can run
## ```r
## obj$workers$send_message("TIMEOUT_SET", 0)
## ```

## which will mean that the workers will stop immediately after not
## recieving a task (so after they finish processing all your jobs
## they'll stop one by one).  Practically this still takes one minute
## because that's the polling timeout time (I may be able to improve
## this later).
obj$stop_workers()
obj$workers$workers_log_tail(workers, n = Inf)

## ## rrq

## In this model, we create a very lightweight queue which in turn
## creates very lightweight tasks.  This avoids even more overhead
## than the approach above, though it can be more difficult to debug
## because less information is saved.  Rather than round-tripping data
## through the disk, everything goes via the redis server.

## The other difference here is that jobs can be run on the cluster
## using the same approach, so you can submit a task that controlls
## some (potentially very large) number of workers, submitting and
## collecting tasks from them.

## The first part here looks very similar
config <- didehpc::didehpc_config(use_rrq = TRUE)
obj <- didehpc::queue_didehpc(ctx, config = config)

## We still submit workers
workers <- obj$submit_workers(5)
workers

## echo = FALSE
Sys.sleep(0.5)

## To send tasks to these workers we need a *second* type of queue:
rrq <- obj$rrq_controller()
rrq

## This will look and act a lot like the main didehpc queue
## controller, but with a few differences.  Tasks will come back as
## plain strings rather than user-friendly objects and `lapply` and
## `enqueue_bulk` are now blocking operations.  The payback for this
## is potentially very fast task turnarounds and better behaviour with
## the disk under heavy load.
t <- rrq$enqueue(sin(1))
rrq$task_wait(t, 10)

## For example; submitting 50 trivial tasks to our pool of five
## workers and retrieving the results:
system.time(res <- rrq$lapply(1:50, sin))

## or 500 tasks:
system.time(res <- rrq$lapply(1:500, sin))

## Across the network, the latency here is ~1/1000 s per task.  On
## fi--didemrchnb it will hopefully be a bit faster because of the
## infiniband network.

## To use this, we'll submit workers as above, then submit a job that
## will use the workers.

##+ echo = FALSE, results = "asis"
writeLines(c("```r", readLines("mysources-rrq.R"), "```"))

## In this code, there is a function `simulation` that takes `nsteps`
## steps and in each steps samples `nsamples` random numbers.  The
## function used to do this is called `slow_rnorm` -- imagine that
## this is some hard to sample from distribution.  The pattern here is
## that the simulation has both a serial and a parallel component;
## each iteration a bunch of things happen that can be done at once
## (the samples) but between steps a naturally serial operation
## happens (sorting the numbers).  This is a contrived example but
## this pattern is fairly common in practice.
ctx <- context::context_save(root,
                             sources = "mysources-rrq.R",
                             unique_value = unique_value)
config <- didehpc::didehpc_config(use_rrq = TRUE)
obj <- didehpc::queue_didehpc(ctx, config = config)
obj$submit_workers(5)

##+ echo = FALSE
Sys.sleep(0.5)

## Now, we can submit a task that will use this set of workers
t <- obj$enqueue(simulation(15, 5))
res <- t$wait(120)
res

## Let's scale it up; suppose we want 100 samples per step:
t2 <- obj$enqueue(simulation(15, 100))

## This is going to take 20x longer!  But there's space on the cluster:
obj$cluster_load(nodes = FALSE)

## So let's submit a bunch more workers
obj$submit_workers(45)

obj$workers$workers_len()
obj$workers$workers_list()
obj$workers$workers_status()

res <- t2$wait(120)
res

## Turn off all our workers
obj$workers$workers_stop()
