## ---
## title: "R and the DIDE cluster, troubleshooting"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output:
##   rmarkdown::html_vignette:
##     toc: true
##     toc_depth: 2
## vignette: >
##   %\VignetteIndexEntry{R and the DIDE cluster, troubleshooting}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo = FALSE,results = "hide"
source("common.R")
r_output <- function(x) {
  cat(c("```r", x, "```"), sep="\n")
}

## Base queue object:
ctx <- context::context_save("contexts")
obj <- didehpc::queue_didehpc(ctx)

## # My job has failed

## ## My job status is `ERROR`

## ### Caused by an error in your code

## If your job status is `ERROR` that *probably* indicates an error in
## your code.  There are lots of reasons that this could be for, and
## the first challenge is working out what happened.
t <- obj$enqueue(mysimulation(10))

##+ echo = FALSE, results = "hide", error = TRUE
t$wait(10)

## This job will fail, and `$status()` will report `ERROR`
t$status()

## The first place to look is the result of the job itself.  Unlike an
## error in your console, an error that happens on the cluster can be
## returned and inspected:
t$result()

## In this case the error is because the function `mysimulation` does
## not exist.

## The other place worth looking is the job log
t$log()

## Sometimes there will be additional diagnostic information there.

## Here's another example:
t <- obj$enqueue(read.csv("c:/myfile.csv"))

##+ echo = FALSE, results = "hide", error = TRUE
t$wait(10)

## This job will fail, and `$status()` will report `ERROR`
t$status()

## Here is the error, which is a bit less informative this time:
t$result()

## The log gives a better idea of what is going on - the file
## `c:/myfile.csv` does not exist (because it is not found on the
## cluster; using relative paths is much preferred to absolute paths)
t$log()

## The real content of the error message is present in the warning!
## You can also get the warnings with
t$result()$warnings

## Which will be a list of all warnings generated during the execution
## of your task (even if it succeeds).  The traceback also shows what
## happened:
t$result()$trace

## ### Caused by an error during startup

## These are harder to troubleshoot but we can still pull some
## information out.  The example here was a real-world case and
## illustrates one of the issues with using a shared filesystem in the
## way that we do here.

##+ echo = FALSE,results = "hide"
writeLines("times2 <-function(x) {\n  2 * x\n}", "mycode.R")

## Suppose you have a context that uses some code in `mycode.R`:
##+ echo = FALSE, results = "asis"
r_output(readLines("mycode.R"))

## You create a cluster:
ctx <- context::context_save("contexts", sources = "mycode.R")
obj <- didehpc::queue_didehpc(ctx)

## Everything seems to work fine:
t <- obj$enqueue(times2(10))
t$wait(10)

## ...but then you're editing the file and save the file but it is not
## syntactically correct:
##
##+ echo = FALSE,results = "hide"
writeLines("times2 <-function(x) {\n  2 * x\n}\nnewfun <- function(x)",
           "mycode.R")
##+ echo = FALSE, results = "asis"
r_output(readLines("mycode.R"))

## And then you either submit a job, **or** a job that you have
## previously submitted gets run (which could happen ages after you
## submit it if the cluster is busy).
t <- obj$enqueue(times2(10))
t$wait(10)
t$status()

## The error here has happened before getting to your code - it is
## happening when context loads the source files.  The log makes this
## a bit clearer:
t$log()

## ### My jobs are getting stuck at `PENDING`

## This is the most annoying one, and can happen for many reasons.
## You can see via the [web interface](mrcdata.dide.ic.ac.uk/hpc/) or
## the Microsoft cluster tools that your job has failed but `didehpc`
## is reporting it as pending.  This happens when something has failed
## during the script that runs *before* any `didehpc` code runs on the
## cluster.

## Things that have triggered this situation in the past:

## * An error in the Microsoft cluster tools
## * A misconfigured node (sometimes they are missing particular software)
## * A networking issue
## * Gremlins
## * Network path mapping error

## There are doubtless others.  Here, I'll simulate one so you can see
## how to troubleshoot it.  I'm going to *deliberately* misconfigure
## the network share that this is running on so that the cluster will
## not be able to map it and the job will fail to start
home <- didehpc::path_mapping("home", getwd(),
                              "//fi--wronghost/path", "Q:")

## The host `fi--wronghost` does not exist so things will likely fail
## on startup.
config <- didehpc::didehpc_config(home = home)
ctx <- context::context_save("contexts")
obj <- didehpc::queue_didehpc(ctx, config)

## Submit a job:
t <- obj$enqueue(sessionInfo())

## And wait...
##+ error = TRUE
t$wait(10)

## It's never going to succeed and yet it's status will stay as:
t$status()

## To get the log from the DIDE cluster you can run:
obj$dide_log(t)

## which here indicates that the network path was not found (because
## it was wrong!)
##
## You can also update any incorrect statuses by running:
obj$task_status_dide()

## Which will print information about anything that was adjusted.
