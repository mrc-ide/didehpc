##' Create a queue object
##'
##' Queues are R6 objects with many methods.  They need documenting still.
##' @title Create a queue object
##'
##' @param context A context
##'
##' @param config Optional dide configuration information.
##'
##' @param root A root directory, not usually needed
##'
##' @param initialise Passed through to the base queue. If you set
##'   this to `FALSE` you will not be able to submit jobs. By default
##'   if `FALSE` this also sets `provision` to `later` and `login` to
##'   `FALSE`.
##'
##' @param provision A provisioning strategy to use. Options are
##' * `lazy`: (the default) which installs packages if they are not
##'   present (note that this differs slightly to the interpretation
##'   of "lazy" used by `pkgdepends` as we try and be even lazier by
##'   not calling `pkgdepends` if it looks like your packages might be
##'   ok - this avoids fetching package metadata - you might want to
##'   run `$provision_context("lazy")` later.
##' * `upgrade`: which tells `pkgdepends` to always try and upgrade
##' * `later`: don't do anything on creation
##' * `fake`: don't do anything but mark the queue as being already
##'   provisioned (this option can come in useful if you really don't
##'   want to risk any accidental package installation)
##'
##' @param login Logical, indicating if we should immediately
##'   login. If `TRUE`, then you will be prompted to login
##'   immediately, rather than when a request to the web portal is
##'   made.
##'
##' @export
queue_didehpc <- function(context, config = didehpc_config(), root = NULL,
                          initialise = TRUE, provision = NULL,
                          login = NULL) {
  queue_didehpc_$new(context, config, root, initialise,
                     provision %||% initialise, login %||% initialise)
}

queue_didehpc_ <- R6::R6Class(
  "queue_didehpc",
  inherit = queuer:::R6_queue_base,
  public = list(
    config = NULL,
    client = NULL,

    initialize = function(context, config, root, initialise, provision, login,
                          client = NULL) {
      super$initialize(context, root, initialise)
      self$config <- as_didehpc_config(config)

      path_root <- self$context$root$path
      private$data <- batch_data(path_root, self$context$id, self$config)

      if (is.null(client)) {
        client <- web_client$new(self$config$credentials,
                                 self$config$cluster,
                                 login)
      }
      self$client <- client

      provision <- provision_policy(provision)
      private$lib <- queue_library$new(
        private$data, self$config$cluster, self$client)
      self$provision_context(provision, quiet = TRUE)
    },

    login = function(refresh = TRUE) {
      self$client$login(refresh)
    },

    cluster_load = function(cluster = NULL, nodes = TRUE) {
      self$client$load_show(cluster, nodes)
    },

    reconcile = function(task_ids = NULL) {
      reconcile(self, task_ids)
    },

    submit = function(task_ids, names = NULL) {
      if (!private$provisioned) {
        stop("Queue is not provisioned; run '$provision_library()'")
      }
      if (self$config$use_workers) {
        rrq_submit_context_tasks(self$config, self$context, task_ids, names)
      } else {
        submit_dide(self, private$data, task_ids, names)
      }
    },

    submit_workers = function(n, timeout = 600, progress = NULL) {
      rrq_submit_workers(self, private$data, n, timeout, progress)
    },

    stop_workers = function(worker_ids = NULL) {
      rrq_stop_workers(self$config, self$context$id, worker_ids)
    },

    rrq_controller = function() {
      rrq_controller(self$config, self$context$id)
    },

    unsubmit = function(task_ids) {
      unsubmit_dide(self, task_ids) # TODO: used to map to id here
    },

    dide_id = function(t) {
      task_ids <- task_get_id(t)
      self$db$mget(task_ids, "dide_id")
      setNames(vcapply(task_ids, self$db$get, "dide_id"), names(task_ids))
    },

    dide_log = function(t) {
      dide_id <- self$dide_id(t)
      assert_scalar_character(dide_id, "task_id")
      cluster <- self$db$get(task_get_id(t), "dide_cluster")
      self$client$log(dide_id, cluster)
    },

    provision_context = function(policy = "lazy", dryrun = FALSE,
                                 quiet = FALSE, show_progress = NULL,
                                show_log = TRUE) {
      policy <- provision_policy(policy)
      if (policy == "later") {
        return()
      }
      if (policy != "fake") {
        dat <- context_packages(self$context,
                                self$config$use_rrq || self$config$use_workers)
        self$install_packages(dat$packages, dat$repos, policy, dryrun, quiet,
                              show_progress, show_log)
      }
      private$provisioned <- TRUE
    },

    install_packages = function(packages, repos = NULL,
                                policy = "lazy", dryrun = FALSE,
                                quiet = FALSE, show_progress = NULL,
                                show_log = TRUE) {
      complete <- private$lib$check(packages)$complete
      if (complete && policy == "lazy") {
        if (!quiet) {
          message("Nothing to install; try running with policy = 'upgrade'")
        }
        return()
      }
      message("Running installation script on cluster")
      private$lib$provision(packages, repos, policy, dryrun,
                            show_progress %||% interactive(), show_log)
      message("Done!")
    }
  ),

  private = list(
    lib = NULL,
    data = NULL,
    provisioned = FALSE
  ))


submit_dide <- function(obj, data, task_ids, names) {
  db <- obj$db
  config <- obj$config
  client <- obj$client
  batch_template <- data$templates$runner

  names <- setNames(task_names(task_ids, names), task_ids)

  ## Will be shared across all jobs submitted
  job_template <- config$template
  cluster <- config$cluster
  resource_type <- config$resource$type
  resource_count <- config$resource$count

  ## TODO: in theory this can be done in bulk on the cluster but it
  ## requires some support on the web interface as we do not get
  ## access to multiple names via the form.
  p <- queuer::progress_timeout(length(task_ids), Inf, label = "submitting ")
  dir.create(data$paths$local$batch, FALSE, TRUE)
  dir.create(data$paths$local$log, FALSE, TRUE)
  for (id in task_ids) {
    base <- paste0(id, ".bat")
    batch <- file.path(data$paths$local$batch, base)
    writeLines(glue_whisker(batch_template, list(task_id = id)), batch)
    path <- windows_path(file.path(data$paths$remote$batch, base))
    p()
    dide_id <- client$submit(path, names[[id]], job_template, cluster,
                             resource_type, resource_count)
    db$set(id, dide_id, "dide_id")
    db$set(id, config$cluster, "dide_cluster")
    db$set(id, path_logs(NULL), "log_path")
  }
}


unsubmit_dide <- function(obj, task_ids) {
  db <- obj$db
  client <- obj$client

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
      cluster <- db$get(id, "dide_cluster")
      ret[[i]] <- client$cancel(dide_id, cluster)
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


task_get_id <- function(x) {
  if (inherits(x, "queuer_task")) {
    task_ids <- x$id
  } else if (inherits(x, "task_bundle")) {
    task_ids <- x$ids
  } else if (is.character(x)) {
    task_ids <- x
  } else {
    stop("Can't determine task id")
  }
  task_ids
}


## What packages do we need?
context_packages <- function(context, need_rrq = FALSE) {
  ## 'callr' is needed if using 'rrq' because the rrq/context queue
  ## runs things in separate processes using callr, but this is only
  ## an optional dependency.
  list(packages = unique(c("context",
                           if (need_rrq) c("rrq", "callr"),
                           context$packages$attached,
                           context$packages$loaded,
                           context$package_sources$packages)),
       repos = c(context$package_sources$repos,
                 didehpc = "https://mrc-ide.github.io/didehpc-pkgs"))
}


task_names <- function(task_ids, names) {
  if (is.null(names)) {
    names <- task_ids
  } else if (length(names) == length(task_ids)) {
    names <- sprintf("%s (%s)", names, task_ids)
  } else {
    stop("incorrect length names")
  }
  names
}
