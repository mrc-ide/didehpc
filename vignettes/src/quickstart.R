## ---
## title: "Quickstart for R and the DIDE cluster"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{Quickstart for R and the DIDE cluster}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

### If knitr fails, you'll get an error
###
### Error in file(file, ifelse(append, "a", "w")) :
###   cannot open the connection
### Calls: <Anonymous> ... handle -> withCallingHandlers -> withVisible -> eval -> eval
### In addition: Warning message:
### In file(file, ifelse(append, "a", "w")) :
###   cannot open file 'quickstart.md': Permission denied
###
### With no information as to where the chunk that fails is.  Running
### in interactive mode can help.  Have fun debugging!
##+ echo=FALSE,results="hide"
knitr::opts_chunk$set(error=FALSE)
set.seed(1)

## > Get yourself running R jobs on the cluster in 10 minutes or so.

## Assumptions that I make here:

## * you are using R
##
## * your task can be represented as running a function on some inputs
##   to create an output (a file based output is OK)
##
## * you are working on a network share and have this mounted on your
##   computer
##
## * you know what packages your code depends on
##
## * your package dependencies are all on CRAN, and are all available
##   in windows binary form.

## If any of these do not apply to you, you'll probably need to read
## the full vignette.  In any case the full vignette contains a bunch
## more information anyway.

## ## Install a lot of packages

## On windows in particular, this step can be infuriating if something
## triggers an upgrade in a package that is being depended on.  If you
## end up in a situation where packages simply can't be loaded you may
## need to restart R and try installing things from scratch.
## Hopefully I can come up with a way of reducing the pain here.

##+ eval=FALSE
install.packages("devtools")
devtools::install_github(c(
  "richfitz/ids",
  "dide-tools/context",
  "richfitz/queuer",
  "dide-tools/didewin"))

## Or:
##+ eval=FALSE
drat:::add("richfitz")
install.packages("didewin")

## Or:
##+ eval=FALSE
install.packages("didewin", repos=c(CRAN="https://cran.rstudio.com",
                                    drat="https://richfitz.github.io/drat"))

## ## Describe your computer so we can find things

## On windows if you are using a domain machine, you should need only
## to select the cluster you want to use

##+ eval=FALSE
didewin::didewin_config_global(cluster="fi--didemrchnb")

## Otherwise, and on any other platform you'll need to provide your username:
##+ eval=FALSE
didewin::didewin_config_global(credentials="yourusername",
                               cluster="fi--didemrchnb")

## If you are running Linux we can get both your username and password
## from the file you use to mount your network shares (see the main
## vignette for details)
didewin::didewin_config_global(credentials="~/.smbcredentials",
                               cluster="fi--didemrchnb")

## If this is the first time you have run this package, best to try
## out the login proceedure with:
didewin::web_login()

## ## Describe your project dependencies so we can recreate that on the cluster

## Make a vector of packages that you use in your project:
packages <- c("ape", "MASS")

## And of files that define functions that you ned to run things:
sources <- "mysources.R"

## (if you had a vector here that would be OK too).  The source file
## here is very simple:

##+ result="asis"
writeLines(c("```r", readLines(sources), "```"))

## Then save this together to form a "context".
ctx <- context::context_save("contexts", packages=packages, sources=sources)

## The first argument here, `"contexts"` is the name of a directory
## that we will use to hold a lot of information about your jobs.  You
## don't need (or particularly want) to know what is in here.

## ## Build a queue, based on this context.

## This will prompt you for your password, as it will try and log in.

## It also installs windows versions of all packages within the
## `contexts` directory -- both packages required to get this whole
## system working and then the packages required for your particular
## jobs.

obj <- didewin::queue_didewin(ctx)

## Once you get to this point we're ready to start running things on
## the cluster.  Let's fire off a test to make sure that everything works OK:
t <- obj$enqueue(sessionInfo())

## We can poll the job for a while, which will print a progress bar.
## If the job is returned in time, it will return the result of
## running the function.  Otherwise it will throw an error.
t$wait(120)

## You can use `t$result()` to get the result straight away (throwing
## an error if it is not ready) or `t$wait(Inf)` to wait forever.

## ## Running a single task

## This is just using the `enqueue` function as above.  But it also
## works with functions defined in files passed in as `sources`; here
## the function `make_tree`.
t <- obj$enqueue(make_tree(10))
tree <- t$wait(120)
tree

## The `t` object has a number of other methods you can use:
t

## Get the result from running a task
t$result()

## Get the status of the task
t$status()

## (might also be "PENDING", "RUNNING" or "ERROR"

## Get the original expression:
t$expr()

## Find out how long everything took
t$times()

## You may see negative numbers for "waiting" as the submitted time is
## based on your computer and started/finished are based on the
## cluster.

## And get the log from running the task
t$log()

## There is also a bit of DIDE specific logging that happens before
## this point; if the job fails inexplicably the answer may be in:
obj$dide_log(t)

## ## Running a bunch of tasks

## There are two broad options here;

## 1. Apply a function to each element of a list, similar to `lapply`
## with `queuer::qlapply`
## 2. Apply a function to each row of a data.frame perhaps using each
## column as a different argument with `queuer::enqueue_bulk`

## Suppose we want to make a bunch of trees of different sizes.  This
## would involve mapping our `make_tree` function over a vector of
## sizes:
sizes <- 3:8
grp <- queuer::qlapply(sizes, make_tree, obj, timeout=0)

## By default, `queuer::qlapply` returns a "task_bundle" with an
## automatically generated name.  You can customise the name with the
## `name` argument.

## Get the startus of all the jobs
grp$status()

## Wait until they are all complete and get the results
res <- grp$wait(120)

## The other bulk interface is where you want to run a function over a
## combination of parameters.  Use `queuer::enqueue_bulk` here.
pars <- expand.grid(a=letters[1:3], b=runif(2), c=pi, stringsAsFactors=FALSE)
pars

grp <- queuer::enqueue_bulk(obj, pars, list, do.call=FALSE, timeout=0)

## By default this runs
##
## * `list(a=pars$a[[1]], b=pars$b[[1]], c=pars$c[[1]])`
## * `list(a=pars$a[[2]], b=pars$b[[2]], c=pars$c[[2]])`
## * ...
## * `list(a=pars$a[[6]], b=pars$b[[6]], c=pars$c[[6]])`

res <- grp$wait(120)
res
