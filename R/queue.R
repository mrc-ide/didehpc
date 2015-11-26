##' Create a queue object
##'
##' Queues are R6 objects with many methods.  They need documenting still.
##' @title Create a queue object
##' @param context A context
##' @param config Optional dide configuration information.
##' @export
queue <- function(context, config=didewin_config()) {
  .R6_queue$new(context, config)
}

## This is going to need to be generic; something is going to have to
## provide a set of interfaces we can use to make this generic.
## Ideally this will work with both rrqueue and didewin + context, and
## I'm undecided about what the correct way forward is in making this
## work generally.

## In rrqueue I was *very* strict about having tasks run in particular
## contexts , but here I don't think I care so much.

## Aim for *composition* over inheritence because I do think that will
## be easier to handle here.  It will make the constructor
## particularly nasty though.  Some work can be done with S3 methods
## potentially as I don't think that the underlying work needs to be
## R6.  Could go for a pattern where:
##
## f <- function(x, ...) {
##   UseMethod("x")
## }
## f.R6 <- function(x, ...) {
##   method <- x[["f"]]
##   if (is.function(method)) {
##     method(...)
##   } else {
##     stop("f is not implemented for objects of type ",
##          paste(class(x), collapse=", "))
##   }
## }
##
## which seems reasonable.
##
## Don't try to implement the entire rrqueue interface (which is
## pretty massive) but instead let's start really simple.
##
## Actually, given that the context thing is pretty obsessed with
## paths it might make sense to implement something specific for this
## context + didewin and then I'll see where this goes.
##
## Basic idea here is that the constructor is going to be nasty, and
## the interface will need to provide a lot of hooks.  But the
## original rrqueue design is pretty good so I think as much as
## possible I should try and mimic it.
.R6_queue <-
  R6::R6Class(
    "queue",
    public=
      list(
        config=NULL,
        root=NULL,

        context=NULL,
        workdir=NULL,

        ## On initialisation we should set the context for subsequent
        ## calls; this is going to be handled differently by different
        ## approaches I suspect, but it will need to be set up.
        initialize=function(context, config, login=TRUE, ...) {
          self$config <- config
          self$context <- context
          self$root <- context$root
          self$workdir <- getwd()
          ## TODO: here, we can run most of write_batch to do the
          ## preparation of paths, etc.
          dir.create(path_batch(self$root), FALSE, TRUE)
          dir.create(path_logs(self$root), FALSE, TRUE)
          ## For tracking the cluster/task id of tasks:
          dir.create(path_dide_task_id(self$root), FALSE, TRUE)
          dir.create(path_dide_cluster(self$root), FALSE, TRUE)
          if (login) {
            self$login()
          }
        },

        login=function() {
          web_login(self$config)
        },

        ## There are some things to work out here with groups, but
        ## I'll punt on that until getting this ported over to storr
        ## because that's just going to require a bit more effort than
        ## I want to add.
        ## Tasks:
        tasks_list=function() {
          tasks_list(self)
        },
        tasks_status=function(task_ids=NULL, follow_redirect=FALSE) {
          tasks_status(self, task_ids, follow_redirect)
        },
        tasks_times=function(task_ids=NULL, unit_elapsed="secs") {
          tasks_times(self, task_ids, unit_elapsed)
        },
        task_get=function(task_id) {
          .R6_task$new(self, task_id)
        },
        task_result=function(task_id, follow_redirect=FALSE) {
          task_result(self, task_id)
        },

        ## Queue things up:

        ## NOTE: no equivalent of rrqueue's key_complete, yet.  But
        ## then there's nothing easy to monitor for.
        ##
        ## NOTE: need to implement "group" here.
        enqueue=function(expr, ..., envir=parent.frame(), submit=TRUE) {
          self$enqueue_(substitute(expr), ..., envir=envir, submit=submit)
        },
        ## I don't know that these always want to be submitted.
        enqueue_=function(expr, ..., envir=parent.frame(), submit=TRUE) {
          task <- context::task_save(expr, self$context, envir)
          if (submit) {
            withCallingHandlers(self$submit(task$id),
                                error=function(e) {
                                  message("Deleting task as submission failed")
                                  context::task_delete(task)
                                })
          }
          invisible(.R6_task$new(self, task$id))
        },

        submit=function(task_ids) {
          ## The underlying interface here might shift because this is
          ## pretty bad UI:
          submit(self$root, task_ids, self$config, self$workdir)
        },

        ## Right, so very few things here are actually interfacing
        ## with the cluster.
        set_cluster=function(cluster=NULL) {
          if (is.null(cluster)) {
            cluster <- setdiff(valid_clusters(), self$config$cluster)
          }
          self$config$cluster <- cluster
        },

        load=function(cluster=NULL) {
          if (is.null(cluster)) {
            cluster <- self$config$cluster
          }
          web_shownodes(cluster)
        },

        ## requeue=function(task_id) {
        ## },

        tasks_drop=function(task_ids) {
          task_drop(self, task_ids)
        },

        set_context=function(context) {
          ## Might need to do some work here, so make it a method
          self$context <- context
        }
      ))
