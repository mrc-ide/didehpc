## What we're really looking for here is:
##  ctx      hpc
##  PENDING  RUNNING -> setup, possibly stalled -> update to RUNNING
##  PENDING  ERROR   -> setup, has failed       -> update to ERROR
##  PENDING  CANCELLED -> setup, manually cancelled -> update to CANCELLED
##  RUNNING  ERROR   -> failure that we can't catch -> update to ERROR
##  RUNNING  COMPLETE -> probable failure that has not been caught -> ERROR
##  RUNNING  CANCELLED -> was running, manually cancelled -> update to CANCELLED
reconcile <- function(obj, task_ids = NULL) {
  dat <- reconcile_compare(obj, task_ids)
  reconcile_update(dat, obj$db)
  reconcile_report(dat)
  invisible(dat)
}


reconcile_compare <- function(obj, task_ids = NULL) {
  status_check <- c("PENDING", "RUNNING")

  if (is.null(task_ids)) {
    task_ids <- obj$task_list()
  }
  st_ctx <- obj$task_status(task_ids)
  i <- st_ctx %in% status_check
  if (!any(i)) {
    return(NULL)
  }

  ## Filter these down to interesting ones:
  task_ids <- task_ids[i]
  st_ctx <- st_ctx[i]

  ## Fetch task status from the cluster; this takes a while.  It's
  ## possible that by filtering to exclude "Complete" we could do
  ## better here but the logic is really hairy.
  message("Fetching job status from the cluster...")
  dat <- obj$client$status_user("*", obj$config$cluster)
  message("  ...done")

  i <- match(task_ids, dat$name)
  st_hpc <- dat$status[i]

  ## Any missing tasks we'll set to missing
  st_hpc[is.na(i)] <- "MISSING"

  ## Need to do one additional check here, in the unlikely chance that
  ## the task completes as we run the (fairly slow) jobstatus command.
  i <- st_hpc == "COMPLETE" & st_hpc != st_ctx
  if (any(i)) {
    check <- obj$task_status(task_ids[i])
    j <- !(check %in% status_check)
    if (any(j)) {
      drop <- which(i)[j]
      task_ids <- task_ids[-drop]
      st_ctx <- st_ctx[-drop]
      st_hpc <- st_hpc[-drop]
    }
  }

  st_new <- st_hpc
  st_new[st_hpc %in% c("COMPLETE", "MISSING")] <- "ERROR"

  data_frame(id = unname(task_ids), # task id
             old = unname(st_ctx),  # status known to context
             hpc = unname(st_hpc),  # status reported by cluster
             new = unname(st_new))  # status we will update to
}


reconcile_update <- function(dat, db) {
  if (NROW(dat) == 0) {
    return(NULL)
  }

  id_error <- dat$id[dat$new == "ERROR"]
  n_error <- length(id_error)
  if (n_error > 0L) {
    message(sprintf("manually erroring %s %s",
                    ngettext(n_error, "task", "tasks"),
                    paste(id_error, collapse = ", ")))
    db$mset(id_error, rep("ERROR", n_error), "task_status")
    db$mset(id_error, rep(list(simpleError("Queued job failure")), n_error),
            "task_results")
  }

  id_cancelled <- dat$id[dat$new == "CANCELLED"]
  n_cancelled <- length(id_cancelled)
  if (n_cancelled > 0L) {
    message(sprintf("manually cancelling %s %s",
                    ngettext(n_cancelled, "task", "tasks"),
                    paste(id_cancelled, collapse = ", ")))
    db$mset(id_cancelled, rep("CANCELLED", n_cancelled), "task_status")
  }

  invisible()
}

reconcile_report <- function(dat) {
  if (NROW(dat) == 0L) {
    message("All job statuses look accurate")
    return()
  }

  explain <- function(i, msg) {
    if (any(i)) {
      ids <- paste(sprintf("  - %s", dat$id[i]), collapse = "\n")
      message(sprintf("%s:\n%s", msg, ids))
    }
  }

  explain(dat$hpc == "ERROR" & dat$old == "PENDING",
          "Tasks have failed while context booting")
  explain(dat$hpc == "ERROR" & dat$old == "RUNNING",
          "Tasks have crashed after starting")
  explain(dat$hpc == "MISSING",
          "Tasks have gone missing on the cluster")
  explain(dat$hpc == "COMPLETE",
          "Tasks have started on cluster, unexpectedly stopped")
  explain(dat$hpc == "CANCELLED" & dat$old == "PENDING",
          "Tasks cancelled before starting")
  explain(dat$hpc == "CANCELLED" & dat$old == "RUNNING",
          "Tasks cancelled after starting")
}
