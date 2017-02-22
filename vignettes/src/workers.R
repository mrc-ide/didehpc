## ---
## title: "R and the DIDE cluster, workers"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output:
##   rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{R and the DIDE cluster}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo = FALSE,results = "hide"
knitr::opts_chunk$set(error = FALSE)
set.seed(1)
options(didehpc.credentials = "~/.smbcredentials",
        didehpc.cluster = "fi--dideclusthn")

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

##+ eval = FALSE
install.packages("rrq",
                 repos = c(CRAN = "https://cran.rstudio.com",
                           drat = "https://richfitz.github.io/drat"))

## The context id is used for communication, so if there is a chance
## that someone else may come up with the same context id as you (same
## packages and names of source files) you should create a small bit
## of unique information and pack that into the context.  This will
## change the id and ensure that the tasks will not collide.  Your
## username or project name make a good piece of data to use here.
unique_value <- "rfitzjoh"

##+ echo = FALSE
unique_value <- ids::random_id()

root <- "context"
ctx <- context::context_save(root, packages = "ape", sources = "mysources.R",
                             unique_value = unique_value)

## Then configure and create the queue:
config <- didehpc::didehpc_config(use_workers = TRUE,
                                  cluster = "fi--dideclusthn")
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
obj$submit_workers(5)

## All workers get names in the form `<adjective>_<animal>_<integer>`
## so that you can remember which workers you set off.  They will turn
## off after 10 minutes of inactivity by default (you can tweak this
## with the `worker_timeout` argument to `didehpc_config` or by
## sending a `TIMEOUT_SET` message).

## Submitting jobs works as before, but should hopefully be a little
## faster:
t <- obj$enqueue(make_tree(5))
t$wait(100)

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
obj$workers$get_responses(id, wait = 10)

## Other than that, hopefully everything else continues as normal.  We
## can submit a bunch of jobs and run them using `queuer::qlapply`:
sizes <- 3:8
grp <- queuer::qlapply(sizes, make_tree, obj)

## Task status:
grp$status()

## Collect the results:
res <- grp$wait(120)

## And the logs:
grp$log()

## While workers will turn off automatically, it's polite to turn them
## off as soon as you're done using `obj$stop_workers()`

## Alternatively, after submitting a bunch of jobs you can run
obj$workers$send_message("TIMEOUT_SET", 0)

## which will mean that the workers will stop immediately after not
## recieving a task (so after they finish processing all your jobs
## they'll stop one by one).  Practically this still takes one minute
## because that's the polling timeout time (I may be able to improve
## this later).
obj$stop_workers()
obj$workers$workers_log_tail(n = Inf)
