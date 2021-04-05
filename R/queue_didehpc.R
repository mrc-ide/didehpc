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
##' @export
queue_didehpc <- function(context, config = didehpc_config(), root = NULL,
                          initialise = TRUE) {
  .R6_queue_didehpc$new(context, config, root, initialise)
}

.R6_queue_didehpc <- R6::R6Class(
  "queue_didehpc",
  ## TODO: this needs exporting properly at some point.
  inherit = queuer:::R6_queue_base,
  public = list(
    config = NULL,
    client = NULL,
    templates = NULL,
    workers = NULL,
    rrq = NULL,

    initialize = function(context, config, root, initialise) {
      super$initialize(context, root, initialise)
      self$config <- as_didehpc_config(config)

      ## Will throw if the context is not network accessible.
      path_root <- self$context$root$path
      prepare_path(path_root, config$shares)

      ## These are useful later
      dir.create(path_batch(path_root), FALSE, TRUE)
      dir.create(path_logs(path_root), FALSE, TRUE)

      self$client <- web_client$new(self$config$credentials,
                                    self$config$cluster,
                                    FALSE)
      ## This is needed for both use_workers and use_rrq, will do
      ## nothing if neither are used.  This sets up the `workers` and
      ## `rrq` elements as required.
      ## initialise_rrq(self)

      workdir <- self$config$workdir %||% self$workdir
      self$templates <- batch_templates(path_root, self$context$id,
                                        self$config, workdir)

      ## We may tidy this up as it's quite a long call
      private$lib <- queue_library$new(
        path_library(self$context$root$path, self$config$r_version),
        file.path(self$context$root$path, "conan"),
        self$config$shares,
        self$templates$conan,
        self$config$cluster,
        self$client)

      if (initialise) {
        private$preflight()
      }
    },

    login = function(refresh = TRUE) {
      self$client$login(refresh)
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
      private$preflight()
      if (isTRUE(self$config$use_workers)) {
        self$rrq$queue_submit(task_ids)
      } else {
        submit_dide(obj, task_ids, names)
      }
    },

    unsubmit = function(t) {
      self$login(FALSE)
      if (isTRUE(self$config$use_workers)) {
        ## TODO: unexported function.
        ##
        ## NOTE: This does not unsubmit the *worker* but pulls task ids
        ## out of the local queue.
        self$rrq$queue_unsubmit(task_ids)
      } else {
        unsubmit_dide(obj, task_ids)
      }
    },

    submit_workers = function(n, timeout = 600, progress = NULL) {
      private$preflight()
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

    rrq_controller = function() {
      ## con <- rrq_redis_con(self$config)
      ## rrq::rrq_controller(self$context, rrq_redis_con(self$config))
      ## self$rrq %||% stop("rrq is not enabled")
    },

    provision_context = function(policy = "skip", dryrun = FALSE) {
      packages <- c(self$context$packages$loaded,
                    self$context$packages$attached,
                    "context",
                    self$context$package_sources$packages)
      repos <- c(self$context$package_sources$repos,
                 didehpc = "https://mrc-ide.github.io/didehpc-pkgs")
      private$install_packages(packages, repos, policy, dryrun)
      private$provisioned <- TRUE
    },

    install_packages = function(packages, repos = NULL,
                                policy = "skip", dryrun = FALSE) {
      complete <- self$lib$check(packages)$complete
      if (complete && policy == "skip") {
        return()
      }
      self$lib$provision(packages, repos, policy, NULL, dryrun)
    }
  ),

  private = list(
    lib = NULL,
    provisioned = NULL,

    preflight = function() {
      self$login(FALSE)
      if (!private$provisioned) {
        self$provision_context("skip")
      }
    }
  ))


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
  ## requires some support on the web interface as we do not get
  ## access to multiple names via the form.
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
