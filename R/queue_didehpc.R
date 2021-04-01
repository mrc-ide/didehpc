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
##' @param root A root directory, not usually needed
##'
##' @param sync A vector of files to sync onto the network share
##'
##' @export
queue_didehpc <- function(context, config = didehpc_config(), root = NULL,
                          initialise = TRUE, sync = NULL) {
  .R6_queue_didehpc$new(context, config, root, initialise, sync)
}

.R6_queue_didehpc <- R6::R6Class(
  "queue_didehpc",
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
      if (!inherits(config, "didehpc_config")) {
        if (is.list(config)) {
          config <- do.call("didehpc_config", config)
        } else {
          stop("Expected a didehpc_config for 'config'")
        }
      }
      super$initialize(context, root, initialise)

      ## This is going to be useful for collecting up information on
      ## things that should be cleaned away.  The context root
      ## contains information on the time of last access.
      if (inherits(context$db$driver, "driver_DBI") &&
          !context$db$exists("info", "didehpc")) {
        info <- list(user = config$username,
                     host = hostname(),
                     path = getwd(),
                     date = Sys.time(),
                     workdir = config$workdir,
                     cluster = config$cluster)
        context$db$set("info", info, "didehpc")
      }

      self$config <- config

      ## Will throw if the context is not network accessible.
      prepare_path(self$context$root$path, config$shares)

      if (is.null(prepare_path(getwd(), self$config$shares, FALSE))) {
        if (!is.null(sync)) {
          assert_character(sync)
        }
        self$sync <- union(self$context$sources, sync)
        if (length(self$sync) > 0L && is.null(config$workdir)) {
          stop("Specify 'workdir' in didehpc config to run out of place")
        }
      } else if (!is.null(sync)) {
        ## There are probably times when this needs relaxing, for
        ## example to synchronise data files that are on a local
        ## computer.
        stop("Do not specify sync if running on a network share")
      }

      dir.create(path_batch(self$context$root$path), FALSE, TRUE)
      dir.create(path_logs(self$context$root$path), FALSE, TRUE)

      if (initialise) {
        self$login()
        self$sync_files()
        self$provision()
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
        self$provision()
      }
    },

    login = function(always = TRUE) {
      if (always || !self$logged_in) {
        if (web_logged_in()) {
          message("Already logged in")
        } else {
          web_login(self$config)
          valid <- web_headnodes()
          if (!(self$config$cluster %in% valid)) {
            web_logout()
            if (length(valid) == 0L) {
              fmt <- "You do not have access to any cluster"
            } else if (length(valid) == 1L) {
              fmt <- "You do not have access to '%s'; try '%s'"
            } else {
              fmt <- "You do not have access to '%s'; try one of %s"
              valid <- paste(squote(valid), collapse = ", ")
            }
            stop(sprintf(fmt, self$config$cluster, valid))
          }
        }
        self$logged_in <- TRUE
      }
    },

    sync_files = function(verbose = TRUE, delete = TRUE) {
      if (length(self$sync) > 0L) {
        check_rsync(self$config)
        wd <- prepare_path(self$config$workdir, self$config$shares)
        dest <- file_path(wd$path_local, wd$rel)
        context::context_log("sync", "Syncronising files")
        syncr::syncr(self$sync, dest, relative = TRUE,
                     verbose = verbose, delete = delete)
      }
    },

    cluster_load = function(cluster = NULL, nodes = TRUE) {
      self$login(FALSE)
      if (isTRUE(cluster)) {
        print(didehpc_load(self$config))
      } else {
        print(didehpc_shownodes(self$config, cluster %||% self$config$cluster),
              nodes = nodes)
      }
    },

    task_status_dide = function(task_ids = NULL) {
      self$login(FALSE)
      task_status_dide(self, task_ids)
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

    submit_workers = function(n, timeout = 600, progress = NULL) {
      self$preflight()
      submit_workers(self, n, timeout, progress)
    },

    stop_workers = function(worker_ids = NULL) {
      self$rrq$worker_stop(worker_ids)
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
      didehpc_joblog(self$config, dide_task_id)
    },

    ## TODO: These could check that the connection is still OK, but
    ## that's hard to do in general.  People should just not serialise
    ## these objects!  I might remove these in favour of just hitting
    ## the fields directly.
    rrq_controller = function() {
      self$rrq %||% stop("rrq is not enabled")
    },

    worker_controller = function() {
      .Deprecated("$rrq_controller", old = "$worker_controller")
      self$rrq %||% stop("workers are not enabled")
    },

    provision = function(installed_action = "upgrade", refresh_drat = FALSE) {
      browser()
      initialise_cluster_packages(self, installed_action, refresh_drat)
    }
  ))


initialise_templates <- function(obj) {
  workdir <- obj$config$workdir %||% obj$workdir
  obj$templates <- batch_templates(obj$context, obj$config, workdir)
}

## TODO: It would be heaps nicer if there was per-context log
## directory setting...
submit <- function(obj, task_ids, names) {
  if (isTRUE(obj$config$use_workers)) {
    ## This is not generally going to be a reasonable thing to do, but
    ## until I get things nailed down, some sort of pause here is
    ## necessary or the jobs get consumed too quickly.
    if (obj$db$driver$type() == "rds") {
      message("sleeping in the hope of a disk sync")
      Sys.sleep(2)
    }
    obj$rrq$queue_submit(task_ids)
  } else {
    submit_dide(obj, task_ids, names)
  }
}

submit_dide <- function(obj, task_ids, names) {
  db <- obj$db
  root <- obj$context$root$path
  config <- obj$config
  template <- obj$templates$runner

  if (is.null(names)) {
    names <- setNames(task_ids, task_ids)
  } else if (length(names) == length(task_ids)) {
    names <- setNames(sprintf("%s (%s)", names, task_ids), task_ids)
  } else {
    stop("incorrect length names")
  }

  ## TODO: in theory this can be done in bulk on the cluster but it
  ## requires some support on the web interface I think.
  p <- queuer::progress_timeout(length(task_ids), Inf, label = "submitting ")
  for (id in task_ids) {
    batch <- write_batch(id, root, template, list(task_id = id))
    path <- remote_path(batch, config$shares)
    p()
    dide_id <- didehpc_submit(config, path, names[[id]])
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
    obj$rrq$queue_unsubmit(task_ids)
  } else {
    unsubmit_dide(obj, task_ids)
  }
}

unsubmit_dide <- function(obj, task_ids) {
  db <- obj$db
  dide_id <- vcapply(task_ids, db$get, "dide_id")
  dide_cluster <- vcapply(task_ids, db$get, "dide_cluster")
  config <- obj$config

  p <- queuer::progress_timeout(length(task_ids), Inf, label = "cancelling ")
  ret <- character(length(task_ids))
  for (i in seq_along(task_ids)) {
    p()
    id <- task_ids[[i]]
    st <- tryCatch(db$get(id, "task_status"),
                   KeyError = function(e) NULL)
    ## Only try and cancel tasks if they seem cancellable:
    if (!is.null(st) && st %in% c("RUNNING", "PENDING")) {
      dide_id <- db$get(id, "dide_id")
      ## TODO: should alter the cluster here?  For now assumes that this
      ## is not needed.
      ##   config$cluster <- db$get(id, "dide_cluster")
      ret[[i]] <- didehpc_cancel(config, dide_id)
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

check_rsync <- function(config) {
  requireNamespace("syncr", quietly = TRUE) ||
    stop("Please install syncr; see https://mrc-ide.github.io/didehpc")
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
  } else if (is.null(x) && is.recursive(obj) && is.function(obj$task_list)) {
    task_ids <- obj$task_list()
    names(task_ids) <- task_ids
  } else {
    stop("Can't determine task id")
  }
  task_ids
}
