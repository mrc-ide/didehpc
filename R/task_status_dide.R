## What we're really looking for here is:
##  ctx      hpc
##  PENDING  RUNNING -> setup, possibly stalled -> update to RUNNING
##  PENDING  ERROR   -> setup, has failed       -> update to ERROR
##  PENDING  CANCELLED -> setup, manually cancelled -> update to CANCELLED
##  RUNNING  ERROR   -> failure that we can't catch -> update to ERROR
##  RUNNING  COMPLETE -> probable failure that has not been caught -> ERROR
##  RUNNING  CANCELLED -> was running, manually cancelled -> update to CANCELLED
task_status_dide <- function(obj, task_ids = NULL) {
  dat <- task_status_dide_compare(obj, task_ids)
  task_status_dide_update(obj, dat)
  task_status_dide_report(dat)
  invisible(dat)
}

task_status_dide_compare <- function(obj, task_ids = NULL) {
  status_check <- c("PENDING", "RUNNING")

  if (is.null(task_ids)) {
    task_ids <- obj$task_list()
  }
  st_ctx <- obj$task_status(task_ids)
  db <- obj$db
  i <- st_ctx %in% status_check
  if (!any(i)) {
    return(data.frame(id = character(0), old = character(0), hpc = character(0),
                      new = character(0), stringsAsFactors = FALSE))
  }
  ## Filter these down to interesting ones:
  task_ids <- task_ids[i]
  st_ctx <- st_ctx[i]

  ## Fetch task status from the cluster; this takes a while.  It's
  ## possible that by filtering to exclude "Complete" we could do
  ## better here but the logic is really hairy.
  message("Fetching job status from the cluster...")
  dat <- obj$client$status_user(obj$config$cluster)
  message("  ...done")

  i <- match(task_ids, dat$name)
  if (any(is.na(i))) {
    stop("Did not find information on tasks: ",
         paste(task_ids[is.na(i)], collapse = ", "))
  }
  st_hpc <- dat$status[i]
  st_new <- dat$status[i]

  ## Need to do one additional check here, in the unlikely chance that
  ## the task completes as we run the (fairly slow) jobstatus command.
  i <- st_hpc == "COMPLETE" & st_ctx %in% status_check
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
  ## These are not graceful exits, though they're hard to do without
  ## being malicious.
  st_new[st_hpc == "COMPLETE"] <- "ERROR"
  data.frame(id = unname(task_ids),
             old = unname(st_ctx),
             hpc = unname(st_hpc),
             new = unname(st_new),
             stringsAsFactors = FALSE)

}

task_status_dide_update <- function(obj, dat) {
  db <- obj$db

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
}

task_status_dide_report <- function(dat) {
  if (nrow(dat) == 0L) {
    message("All job statuses look accurate")
    return()
  }

  id <- dat$id
  ctx <- dat$old
  hpc <- dat$hpc

  i <- hpc == "ERROR" & ctx == "PENDING"
  if (any(i)) {
    message("Tasks have failed while context booting:\n",
            paste(sprintf("\t- %s", id[i]), collapse = "\n"))
  }

  i <- hpc == "ERROR" & ctx == "RUNNING"
  if (any(i)) {
    message("Tasks have crashed after starting:\n",
            paste(sprintf("\t- %s", id[i]), collapse = "\n"))
  }

  i <- hpc == "COMPLETE"
  if (any(i)) {
    message("Tasks have started un cluster, unexpectedly stopped:\n",
            paste(sprintf("\t- %s", id[i]), collapse = "\n"))
  }

  i <- hpc == "CANCELLED" & ctx == "PENDING"
  if (any(i)) {
    message("Tasks cancelled before starting:\n",
            paste(sprintf("\t- %s", id[i]), collapse = "\n"))
  }
  i <- hpc == "CANCELLED" & ctx == "RUNNING"
  if (any(i)) {
    message("Tasks cancelled after starting:\n",
            paste(sprintf("\t- %s", id[i]), collapse = "\n"))
  }
}
