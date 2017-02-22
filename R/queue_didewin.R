##' Create a queue object
##'
##' Queues are R6 objects with many methods.  They need documenting still.
##' @title Create a queue object
##'
##' @param context A context
##'
##' @param config Optional dide configuration information.
##'
##' @param initialise initialise context and log in to the cluster on
##'   queue creation (recommended on initial creation, but not
##'   required if you want to check on jobs).
##'
##' @export
##' @importFrom provisionr package_sources
queue_didewin <- function(context, config = didewin_config(), root = NULL,
                          initialise = TRUE, sync = NULL) {
  .R6_queue_didewin$new(context, config, root, initialise, sync)
}

.R6_queue_didewin <- R6::R6Class(
  "queue_didewin",
  ## TODO: this needs exporting properly at some point.
  inherit = queuer:::R6_queue_base,
  public = list(
    config = NULL,
    logged_in = FALSE,
    provisioned = FALSE,
    sync = NULL,
    templates = NULL,
    workers = NULL,
    rrq = NULL,

    initialize = function(context, config, root, initialise, sync) {
      if (!inherits(config, "didewin_config")) {
        stop("Expected a didewin_config for 'config'")
      }
      super$initialize(context, root, initialise)

      self$config <- config
      self$config$rtools <- needs_rtools(self$config, self$context)

      ## Will throw if the context is not network accessible.
      prepare_path(self$context$root$path, config$shares)

      if (is.null(prepare_path(getwd(), self$config$shares, FALSE))) {
        if (!is.null(sync)) {
          assert_character(sync)
        }
        self$sync <- union(self$context$sources, sync)
        if (length(self$sync) > 0L && is.null(config$workdir)) {
          stop("Specify 'workdir' in didewin config to run out of place")
        }
      } else if (!is.null(sync)) {
        ## There are probably times when this needs relaxing, for
        ## example to synchronise data files that are on a local
        ## computer.
        stop("No not specify sync if running on a network share")
      }

      dir.create(path_batch(self$context$root$path), FALSE, TRUE)
      dir.create(path_logs(self$context$root$path), FALSE, TRUE)

      if (use_hpctools(self$config)) {
        self$logged_in <- TRUE
      } else if (initialise) {
        self$login()
      }

      if (initialise) {
        self$sync_files()
        initialise_cluster_packages(self)
      }

      ## This is needed for both use_workers and use_rrq, will do
      ## nothing if neither are used.  This sets up the `workers` and
      ## `rrq` elements as required.
      initialise_rrq(self)

      ## NOTE: templates need to be done last because any of the bits
      ## above might alter things that are used in constructing the
      ## templates.
      initialise_templates(self)
    },

    ## Similar to login() but tests for package initialisation too.
    ## As I fix up the file sync stuff, I'll see if this belongs in
    ## here too, or if it should be moved elsewhere.  It's not clear
    ## that it should happen _every_ job submission (because that gets
    ## a bit heavy on the IO) but it should happen _at least_ once on
    ## queue startup.
    preflight = function() {
      self$login(FALSE)
      if (!self$provisioned) {
        ## TODO: this is not quite the right place to put the syncing,
        ## but the idea is right.  It probably just needs its own
        ## conditional.
        self$sync_files()
        initialise_cluster_packages(self)
      }
    },

    login = function(always = TRUE) {
      if (always || !self$logged_in) {
        if (web_logged_in()) {
          message("Already logged in")
        } else {
          web_login(self$config)
        }
        self$logged_in <- TRUE
      }
    },

    sync_files = function(verbose = TRUE, delete = TRUE) {
      if (length(self$sync) > 0L) {
        check_rsync(self$config)
        ## TODO: save self$config$workdir as a prepared path?
        wd <- prepare_path(self$config$workdir, self$config$shares)
        dest <- file_path(wd$path_local, wd$rel)
        message("Syncronising files")
        ## TODO: this should check that everything exists below the
        ## current directory and arrange to sync it *relatively*.
        ## There's no point synchronising paths that are elsewhere
        ## because that's not going to automatically remap paths
        ## correctly.
        syncr::syncr(self$sync, dest, verbose = verbose, delete = delete)
      }
    },

    didehpc_load = function() {
      self$login(FALSE)
      print(didewin_load(self$config))
    },

    cluster_load = function(cluster = NULL, nodes = TRUE, all = FALSE) {
      self$login(FALSE)
      print(didewin_shownodes(self$config, cluster %||% self$config$cluster),
            nodes = nodes)
    },

    tasks_status_dide = function(task_ids = NULL) {
      self$login(FALSE)
      check_tasks_status_dide(self, task_ids)
    },

    submit = function(task_ids, names = NULL) {
      self$preflight()
      ## See below:
      submit(self, task_ids, names)
    },

    unsubmit = function(t) {
      self$login(FALSE)
      ## TODO: The task_get_id functionality would be nice throughout
      ## the class, probably at the base level.  But need to be
      ## careful to get it really consistent or it will just be
      ## confusing.
      ##
      ## Also would be really nice in delete at queue_base level
      unsubmit(self, task_get_id(t))
    },

    submit_workers = function(n, timeout = 600, progress = TRUE) {
      self$preflight()
      submit_workers(self, n, timeout, progress)
    },

    stop_workers = function(worker_ids = NULL) {
      self$workers$workers_stop(worker_ids)
    },

    dide_id = function(t) {
      task_ids <- task_get_id(t, self)
      db <- self$db
      db$mget(task_ids, "dide_id")
      setNames(vcapply(task_ids, db$get, "dide_id"), names(task_ids))
    },

    dide_log = function(t) {
      self$login(FALSE)
      dide_task_id <- self$dide_id(t)
      assert_scalar_character(dide_task_id, "task_id") # bit of trickery
      didewin_joblog(self$config, dide_task_id)
    },

    ## TODO: These could check that the connection is still OK, but
    ## that's hard to do in general.  People should just not serialise
    ## these objects!  I might remove these in favour of just hitting
    ## the fields directly.
    rrq_controller = function() {
      self$rrq %||% stop("rrq is not enabled")
    },

    worker_controller = function() {
      self$workers %||% stop("workers are not enabled")
    }
  ))

## There are two steps here; the first is to see if there are any
## packages that do need building, the second step is to try and build
## them.
##
## A binary package needs building if it is present in the drat but
## there is no binary package that is not later than it.  We could get
## clever and check the hashes and store them in the context db
## perhaps?
##
## TODO: The other thing that is needed is the on-cluster
## initialisation.  Basically we will pass a task with the expression
## `sessionInfo` to the cluster.  That will trigger a full
## installation.  It's really only worth doing that if the packages do
## not appear to be installed though so we will have to do a check
## here.
initialise_cluster_packages <- function(obj) {
  lib_r_platform <- cran_platform(obj$config$cluster)
  lib_r_version <- obj$config$r_version

  if (isTRUE(obj$config$use_rrq) || isTRUE(obj$config$use_workers)) {
    ## Need to ensure that 'rrq' is loaded for the workers to use
    obj$context$packages$loaded <- c(obj$context$packages$loaded, "rrq")
  }

  if (is.null(obj$config$common_lib)) {
    additional_libraries <- NULL
  } else {
    additional_libraries <- file.path(obj$config$common_lib$path_local,
                                      obj$config$common_lib$rel)
  }

  ## TODO: need to get additional arguments passed through here;
  ## installed_action is the key one (quiet might be useful too).
  ##
  ## Something to force refreshing the drat on build too, but that
  ## will best be combined with non-versioned updates based on MD5.
  res <- context::provision_context(
    obj$context, lib_r_platform, lib_r_version, allow_missing = TRUE,
    additional_libraries = additional_libraries)
  if (!is.null(res$missing)) {
    initialise_cluster_packages_build(res, obj$config)
  }

  obj$provisioned <- TRUE
}

initialise_cluster_packages_build <- function(dat, config) {
  message("Trying to build required binary packages; may take a minute")
  loadNamespace("buildr")

  missing <- dat$missing # or dat$db$src[dat$missing, , drop = FALSE]
  path_lib <- dat$path_lib

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  buildr_host <- config$build_server
  buildr_port <-
    as.integer(paste(c("87", unclass(config$r_version)[[1]][1:2]),
                     collapse = ""))

  url <- sprintf("%s/%s_%s.%s", missing[, "Repository"],
                 missing[, "Package"], missing[, "Version"], "tar.gz")
  for (u in url) {
    download.file(u, file.path(tmp, basename(u)))
  }
  ## TODO: this is a 1 hour timeout which seems more than enough.  I
  ## think that we could make this configurable pretty happily but
  ## we'd want to do that via either an option, possibly paired with a
  ## config option.  For now, leaving it as it is.
  bin <- buildr::build_binaries(file.path(tmp, basename(url)),
                                buildr_host, buildr_port,
                                timeout = 3600)

  ## TODO: this somewhat duplicates a little of the cross
  ## installation, but it's not a great big deal really.  We *do*
  ## need to remove existing package directories first though, or
  ## the packages could be inconsistent.  Thankfully getting the
  ## full name is not a huge deal
  extract <- if (linux_cluster(config$cluster)) untar else unzip

  dest_full <- file.path(path_lib, rownames(missing))
  unlink(dest_full[file.exists(dest_full)], recursive = TRUE)

  for (b in bin) {
    extract(b, exdir = path_lib)
  }
}

initialise_packages_on_cluster <- function(obj, timeout = Inf) {
  t <- obj$enqueue_(quote(sessionInfo()))
  message("Initialising packages on the cluster itself")
  message("You'll need to check the status of the job if this doesn't complete")
  ## TODO: This one should poll the actual job status of the task on
  ## the cluster rather than the result in context because the failure
  ## will not be correctly logged.  I should actually tweak that I
  ## think; once we have context loaded there's no reason why a job
  ## cannot error.  This would be really good and not too bad to test.
  t$wait(timeout = timeout)
}

initialise_templates <- function(obj) {
  workdir <- obj$config$workdir %||% obj$workdir
  obj$templates <- batch_templates(obj$context, obj$config, workdir)
}

## TODO: It would be heaps nicer if there was per-context log
## directory setting...
submit <- function(obj, task_ids, names) {
  if (isTRUE(obj$config$use_workers)) {
    obj$workers$queue_submit(task_ids)
  } else {
    submit_dide(obj, task_ids, names)
  }
}

submit_dide <- function(obj, task_ids, names) {
  db <- obj$db
  root <- obj$context$root$path
  config <- obj$config
  template <- obj$templates$runner

  pb <- progress_bar("Submitting", length(task_ids))

  if (is.null(names)) {
    names <- setNames(task_ids, task_ids)
  } else if (length(names) == length(task_ids)) {
    names <- setNames(sprintf("%s (%s)", names, task_ids), task_ids)
  } else {
    stop("incorrect length names")
  }

  linux <- linux_cluster(config$cluster)

  ## TODO: in theory this can be done in bulk on the cluster but it
  ## requires some support on the web interface I think.
  for (id in task_ids) {
    batch <- write_batch(id, root, template, list(task_id = id), linux)
    dat <- prepare_path(batch, config$shares)
    if (linux) {
      path <- file.path(dat$drive_remote, dat$rel, fsep = "/")
    } else {
      path <- remote_path(dat)
    }
    pb()
    dide_id <- didewin_submit(config, path, names[[id]])
    db$set(id, dide_id,        "dide_id")
    db$set(id, config$cluster, "dide_cluster")
    db$set(id, path_logs(NULL), "log_path")
  }
}

unsubmit <- function(obj, task_ids) {
  if (isTRUE(obj$config$use_workers)) {
    ## TODO: unexported function.
    ##
    ## NOTE: This does not unsubmit the *worker* but pulls task ids
    ## out of the local queue.
    obj$workers$queue_unsubmit(task_ids)
  } else {
    unsubmit_dide(obj, task_ids)
  }
}

unsubmit_dide <- function(obj, task_ids) {
  db <- obj$db
  dide_id <- vcapply(task_ids, db$get, "dide_id")
  dide_cluster <- vcapply(task_ids, db$get, "dide_cluster")
  config <- obj$config

  pb <- progress_bar("Cancelling", length(task_ids))

  ret <- character(length(task_ids))
  for (i in seq_along(task_ids)) {
    pb()
    id <- task_ids[[i]]
    st <- tryCatch(db$get(id, "task_status"),
                   KeyError = function(e) NULL)
    ## Only try and cancel tasks if they seem cancellable:
    if (!is.null(st) && st %in% c("RUNNING", "PENDING")) {
      dide_id <- db$get(id, "dide_id")
      ## TODO: should alter the cluster here?  For now assumes that this
      ## is not needed.
      ##   config$cluster <- db$get(id, "dide_cluster")
      ret[[i]] <- didewin_cancel(config, dide_id)
      if (ret[[i]] == "OK") {
        db$set(id, "CANCELLED", "task_status")
        db$set(id, simpleError("Task cancelled"), "task_results")
      }
    } else {
      ret[[i]] <- "NOT_RUNNING"
    }
  }
  ret
}

## What we're really looking for here is:
##  ctx      dide
##  PENDING  RUNNING -> setup, possibly stalled -> update to RUNNING
##  PENDING  ERROR   -> setup, has failed       -> update to ERROR
##  PENDING  CANCELLED -> setup, manually cancelled -> update to CANCELLED
##  RUNNING  ERROR   -> failure that we can't catch -> update to ERROR
##  RUNNING  COMPLETE -> probable failure that has not been caught -> ERROR
##  RUNNING  CANCELLED -> was running, manually cancelled -> update to CANCELLED
check_tasks_status_dide <- function(obj, task_ids = NULL) {
  if (is.null(task_ids)) {
    task_ids <- obj$tasks_list()
  }
  st_ctx <- obj$tasks_status(task_ids)
  db <- obj$db

  i <- st_ctx %in% c("PENDING", "RUNNING", "CANCELLED")
  if (!any(i)) {
    message("No tasks need checking")
    return()
  }
  task_ids <- task_ids[i]
  st_ctx <- st_ctx[i]

  ## Because this is not really API, I'm going through this way so
  ## there's only one place to check:
  set_task_error <- function(id) {
    message("manually erroring task ", id)
    db$set(id, "ERROR", "task_status")
    db$set(id, simpleError("Queued job failure"), "task_results")
  }
  set_task_cancel <- function(id) {
    message(sprintf("marking task %s as cancelled", id))
    db$set(id, "CANCELLED", "task_status")
    db$set(id, simpleError("Task cancelled"), "task_results")
  }

  ## Realistically we're not interested in Finished here, and that does
  ## bank up after a bit.  Talk with Wes about improvements perhaps?
  dat <- didewin_jobstatus(obj$config)
  i <- match(task_ids, dat$name)
  if (any(is.na(i))) {
    stop("Did not find information on tasks: ",
         paste(task_ids[is.na(i)], collapse = ", "))
  }

  ok <- TRUE
  st_dide <- dat$status[i]

  i <- st_dide == "ERROR"
  if (any(i)) {
    j <- i & st_ctx == "PENDING"
    if (any(j)) {
      message("Tasks have failed while context booting:\n",
              paste(sprintf("\t- %s", task_ids[j]), collapse = "\n"))
    }
    j <- i & st_ctx == "RUNNING"
    if (any(j)) {
      message("Tasks have crashed after starting:\n",
              paste(sprintf("\t- %s", task_ids[j]), collapse = "\n"))
    }
    lapply(task_ids[i], set_task_error)
    ok <- FALSE
  }

  i <- st_ctx == "RUNNING" & st_dide == "COMPLETE"
  if (any(i)) {
    ## Second round of check to avoid a race condition:
    f <- function(id) {
      if (obj$tasks_status(id) == "RUNNING") {
        set_task_error(id)
        TRUE
      } else {
        FALSE
      }
    }
    res <- vlapply(task_ids[i], f)
    if (any(res)) {
      message("Tasks have started on cluster, unexpectedly stopped:\n",
              paste(sprintf("\t- %s", task_ids[i][res]), collapse = "\n"))
    }
    ok <- FALSE
  }

  i <- st_dide == "CANCELLED"
  if (any(i)) {
    j <- i & st_ctx == "PENDING"
    if (any(j)) {
      message("Tasks cancelled while context booting:\n",
              paste(sprintf("\t- %s", task_ids[j]), collapse = "\n"))
    }
    j <- i & st_ctx == "RUNNING"
    if (any(j)) {
      message("Tasks cancelled after starting:\n",
              paste(sprintf("\t- %s", task_ids[j]), collapse = "\n"))
    }
    lapply(task_ids[i], set_task_error)
    ok <- FALSE
  }

  i <- st_ctx == "PENDING" & st_dide == "RUNNING"
  if (any(i)) {
    message("Tasks have started on cluster, but context still booting:\n",
            paste(sprintf("\t- %s", task_ids[i]), collapse = "\n"))
    ok <- FALSE
  }

  if (ok) {
    message("Reported statuses seem accurate to me")
  }
  invisible(NULL)
}

check_rsync <- function(config) {
  requireNamespace("syncr", quietly = TRUE) ||
    stop("Please install syncr; see https://dide-tools.github.io/didewin")
  if (!syncr::has_rsync()) {
    if (is_windows()) {
      path_rsync <- file.path(rtools_info(config)$path, "bin", "rsync")
      if (file.exists(path_rsync)) {
        options("syncr.rsync" = path_rsync)
        if (syncr::has_rsync()) {
          return()
        }
      }
    }
    stop("Could not find rsync binary; can't run out of place")
  }
}

## A helper function that will probably move into queue_base
task_get_id <- function(x, obj = NULL) {
  if (inherits(x, "queuer_task")) {
    task_ids <- x$id
  } else if (inherits(x, "task_bundle")) {
    task_ids <- x$ids
  } else if (is.character(x)) {
    task_ids <- x
  } else if (is.null(x) && is.recursive(obj) && is.function(obj$tasks_list)) {
    task_ids <- obj$tasks_list()
    names(task_ids) <- task_ids
  } else {
    stop("Can't determine task id")
  }
  task_ids
}
