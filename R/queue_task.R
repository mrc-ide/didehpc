## Right.  The deal with rrqueue was that we could pass around
## {con,keys} as a way of getting in and extracting information about
## what jobs are where.  Here we'll pass 'obj' as the object and see
## what happens.  `obj` can be anything that satisfies having elements:
##    * root
##    * config
.R6_task <-
  R6::R6Class(
    "task",
    public=
      list(
        root=NULL,
        config=NULL,
        id=NULL,
        handle=NULL,

        initialize=function(obj, id, check_exists=TRUE) {
          self$config <- obj$config
          self$root <- obj$root
          self$id <- id
          self$handle <- context::task_handle(root, id, check_exists)
        },
        status=function(follow_redirect=FALSE) {
          tasks_status(self, self$id, follow_redirect)
        },
        result=function(follow_redirect=FALSE) {
          task_result(self, self$id)
        },
        expr=function(locals=FALSE) {
          task_expr(self, self$id)
        },
        context=function() {
          context::task_read(self$handle)$context_id
        },
        log=function() {
          task_log(self, self$id)
        },
        times=function(unit_elapsed="secs") {
          tasks_times(self, self$id, unit_elapsed)
        },
        wait=function(timeout, every=0.5) {
          task_wait(self, self$id, timeout, every)
        }))

task_result <- function(obj, task_id,
                        follow_redirect=FALSE, sanitise=FALSE) {
  if (follow_redirect) {
    .NotYetUsed("follow_redirect")
  }
  status <- tasks_status(obj, task_id)
  if (status == TASK_COMPLETE || status == TASK_ERROR) {
    context::task_result(context::task_handle(obj$root, task_id, FALSE))
  } else if (sanitise) {
    UnfetchableTask(task_id, status)
  } else {
    stop(sprintf("task %s is unfetchable: %s", task_id, status))
  }
}

## TODO: Does not deal with local expressions yet.
## TODO: not sure
task_expr <- function(obj, task_id) {
  context::task_read(context::task_handle(obj$root, task_id))$expr
}

tasks_list <- function(obj) {
  context::tasks_list(obj$root)
}

tasks_status <- function(obj, task_ids, follow_redirect=FALSE) {
  if (follow_redirect) {
    .NotYetUsed("follow_redirect")
  }
  if (is.null(task_ids)) {
    task_ids <- tasks_list(obj)
  }
  task_handle <- context::task_handle(obj$root, task_ids, TRUE)
  context::task_status_read(task_handle)
}

tasks_times <- function(obj, task_ids=NULL, unit_elapsed="secs") {
  ## TODO: *cache* the times here aginst their status so that we never
  ## look tasks up twice if they have not changed status.  That's not
  ## that hard because we could store tasks against `digest(c(task_id,
  ## task_status))`.
  ##
  ## Probably simplest to wait for storr though (though didewin can
  ## depend on storr easily enough).
  ##
  ## One way of doing this is to write a list with 1, 2, 3 times in it?
  res <- tasks_ours(obj)
  if (!is.null(task_ids)) {
    res[match(task_ids, res$task_id), , drop=FALSE]
  }
  if (nrow(res) == 0L) {
    ## NOTE: can't get a zero length time.
    ret <- data.frame(task_id=character(0),
                      submitted=character(0),
                      started=character(0),
                      finished=character(0),
                      waiting=numeric(0),
                      running=numeric(0),
                      idle=numeric(0),
                      stringsAsFactors=FALSE)
  } else {
    ret <- data.frame(task_id=res$task_id,
                      submitted=res$time_submit,
                      started=res$time_start,
                      finished=res$time_end,
                      stringsAsFactors=FALSE)
    now <- Sys.time()
    started2  <- ret$started
    finished2 <- ret$finished
    finished2[is.na(finished2)] <- started2[is.na(started2)] <- now
    ret$waiting <- as.numeric(started2  - ret$submitted, unit_elapsed)
    ret$running <- as.numeric(finished2 - ret$started,   unit_elapsed)
    ret$idle    <- as.numeric(now       - ret$finished,  unit_elapsed)
  }
  ret
}

## Tasks that we are looking after:
tasks_ours <- function(obj) {
  task_id <- tasks_list(obj)

  ## These are the ones running on dide:
  f_dide_task_id <- path_dide_task_id(obj$root, task_id)
  f_dide_cluster <- path_dide_cluster(obj$root, task_id)

  ok <- file.exists(f_dide_task_id)
  task_id <- task_id[ok]

  dide_task_id <- vcapply(f_dide_task_id[ok], readLines, USE.NAMES=FALSE)
  dide_cluster <- vcapply(f_dide_cluster[ok], readLines, USE.NAMES=FALSE)

  f <- function(cl) {
    dat <- web_jobstatus(obj$config, cl)
    i <- dide_cluster == cl
    id <- dide_task_id[i]
    j <- match(id, dat$dide_task_id)
    cbind(task_id=task_id[i],
          dide_cluster=cl,
          dat[j, , drop=FALSE],
          stringsAsFactors=FALSE)
  }
  ret <- do.call("rbind", lapply(unique(dide_cluster), f))
  ret <- ret[order(ret$time_submit), , drop=FALSE]
  rownames(ret) <- NULL
  ret
}

task_wait <- function(obj, task_id, timeout, every=0.5) {
  t <- time_checker(timeout)
  repeat {
    res <- task_result(obj, task_id, sanitise=TRUE)
    if (!inherits(res, "UnfetchableTask")) {
      return(res)
    } else if (t()) {
      Sys.sleep(every)
    } else {
      stop("task not returned in time")
    }
  }
}

task_drop <- function(obj, task_id) {
  ## TODO: This should also cancel *running* tasks, and that sort of thing.
  context::task_delete(context::task_handle(obj$root, task_ids, FALSE))
}

task_log <- function(obj, task_id) {
  ## I should do some parsing here.
  dat <- readLines(path_logs(obj$root, task_id))
  pretty_context_log(context::parse_context_log(dat))
}

UnfetchableTask <- function(task_id, task_status) {
  structure(list(task_id=task_id,
                 task_status=task_status),
            class=c("UnfetchableTask", "error", "condition"))
}

pretty_context_log <- function(x) {
  yellow <- crayon::make_style("yellow")$bold
  green <- crayon::make_style("blue")$bold
  x$str <- green(x$str)
  i <- vapply(x$body, length, integer(1)) > 0L
  x$body[i] <- lapply(x$body[i], yellow)
  x
}
