##' Create a queue object. This is an [R6::R6Class] object which you
##' interact with by calling "methods" which are described below, and
##' on the help page for [queuer::queue_base], from which this
##' derives.
##'
##' @title Create a queue object
##'
##' @param context A context
##'
##' @param config Optional dide configuration information.
##'
##' @param root A root directory, not usually needed
##'
##' @param initialise Passed through to the base queue. If you set
##'   this to `FALSE` you will not be able to submit tasks. By default
##'   if `FALSE` this also sets `provision` to `later` and `login` to
##'   `FALSE`.
##'
##' @param provision A provisioning strategy to use. Options are
##' * `verylazy` (the default) which installs packages if any declared
##'   package is not present, or if the remote library has already
##'   been provisioned. This is lazier than the `lazy` policy and
##'   faster as it avoids fetching package metadata, which may take a
##'   few seconds. If you have manually adjusted your library
##'   (especially by removing packages) you will probably want to use
##'   `lazy` or `upgrade` to account for dependencies of your declared
##'   packages.
##' * `lazy`: which tells `pkgdepends` to be "lazy" - this prefers to
##'   minimise installation time and does not upgrade packages unless required.
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

##' @rdname queue_didehpc
queue_didehpc_ <- R6::R6Class(
  "queue_didehpc",
  cloneable = FALSE,
  inherit = queuer::queue_base,
  public = list(
    ##' @field config Your [didehpc::didehpc_config()] for this queue.
    ##'   Do not change this after queue creation as changes may not
    ##'   take effect as expected.
    config = NULL,

    ##' @field client A [didehpc::web_client] object used to
    ##'   communicate with the web portal. See the help page for its
    ##'   documentation, but you will typically not need to interact
    ##'   with this.
    client = NULL,

    ##' @description Constructor
    ##'
    ##' @param context,config,root,initialise,provision,login See above
    ##'
    ##' @param client A [didehpc::web_client] object, primarily useful
    ##'   for testing the package
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

    ##' @description Log onto the web portal. This will be called
    ##'   automatically at either when creating the object (by default)
    ##'   or when you make your first request to the portal. However,
    ##'   you can call this to refresh the session too.
    ##'
    ##' @param refresh Logical, indicating if we should try
    ##'   logging on again, even if it looks like we already have.
    ##'   This will refresh the session, which is typically what you want to do.
    login = function(refresh = TRUE) {
      self$client$login(refresh)
    },

    ##' @description Report on the overall cluster usage
    ##'
    ##' @param cluster Cluster to show; if `TRUE` show the entire cluster
    ##'   (via `load_overall`), if `NULL` defaults to the value
    ##'   `config$cluster`
    ##'
    ##' @param nodes Show the individual nodes when printing
    cluster_load = function(cluster = NULL, nodes = TRUE) {
      self$client$load_show(cluster, nodes)
    },

    ##' @description Attempt to reconcile any differences in task state
    ##'   between our database and the HPC queue. This is needed when
    ##'   tasks have crashed, or something otherwise bad has happened
    ##'   and you have tasks stuck in `PENDING` or `RUNNING` that are
    ##'   clearly not happy. This function does not play well with workers and
    ##'   you should not use it if using them.
    ##'
    ##' @param task_ids A vector of tasks to check
    reconcile = function(task_ids = NULL) {
      reconcile(self, task_ids)
    },

    ##' @description Submit a task to the queue. Ordinarily you do not call
    ##'   this directly, it is called by the `$enqueue()` method of
    ##'   [queuer::queue_base] when you create a task. However, you can
    ##'   use this to resubmit a task that has failed if you think it will
    ##'   run successfully a second time (e.g., because you cancelled it
    ##'   the first time around).
    ##'
    ##' @param task_ids A vector of task identifiers to submit.
    ##'
    ##' @param names Optional names for the tasks.
    ##'
    ##' @param names Optional vector of task dependencies, named by task id
    submit = function(task_ids, names = NULL, dpeends_on = NULL) {
      if (!private$provisioned) {
        stop("Queue is not provisioned; run '$provision_library()'")
      }
      if (self$config$use_workers) {
        rrq_submit_context_tasks(self$config, self$context, task_ids, names)
      } else {
        submit_dide(self, private$data, task_ids, names, depends_on)
      }
    },

    ##' @description Submit workers to the queue. This only works if
    ##'   `use_rrq` or `use_workers` is `TRUE` in your configuration.
    ##'    See `vignette("workers")` for more information.
    ##'
    ##' @param n The number of workers to submit
    ##'
    ##' @param timeout The time to wait, in seconds, for all workers to come
    ##'   online. An error will be thrown if this time is exceeded.
    ##'
    ##' @param progress Logical, indicating if a progress bar should be printed
    ##'   while waiting for workers.
    submit_workers = function(n, timeout = 600, progress = NULL) {
      rrq_submit_workers(self, private$data, n, timeout, progress)
    },

    ##' @description Stop workers running on the cluster. See
    ##'   `vignette("workers")` for more information. By default
    ##'   workers will timeout after 10 minutes of inactivity.
    ##'
    ##' @param worker_ids Vector of worker names to try and stop. By
    ##'   default all workers are stopped.
    stop_workers = function(worker_ids = NULL) {
      rrq_stop_workers(self$config, self$context$id, worker_ids)
    },

    ##' @description Return an [rrq::rrq_controller] object, if you have
    ##'   set `use_rrq` or `use_workers` in your configuration. This is
    ##'   a lightweight queue using your workers which is typically much
    ##'   faster than submitting via `$enqueue()`. See `vignette("workers")`
    ##'   for more information.
    rrq_controller = function() {
      didehpc_rrq_controller(self$config, self$context$id)
    },

    ##' @description Unsubmit tasks from the cluster. This removes the tasks
    ##'   from the queue if they have not been started yet, and stops
    ##'   them if currently running. It will have no effect if the tasks
    ##'   are completed (successfully or errored)
    ##'
    ##' @param task_ids Can be a task id (string), a vector of task ids, a task, a
    ##' list of tasks, a bundle returned by enqueue_bulk, or a list of bundles.
    unsubmit = function(task_ids) {
      unsubmit_dide(self, task_ids) # TODO: used to map to id here
    },

    ##' @description Find the DIDE task id of your task. This is the number
    ##'   displayed in the web portal.
    ##'
    ##' @param task_ids Vector of task identifiers to look up
    dide_id = function(task_ids) {
      task_ids <- task_get_id(task_ids)
      self$context$db$mget(task_ids, "dide_id")
      setNames(vcapply(task_ids, self$context$db$get, "dide_id"),
               names(task_ids))
    },

    ##' @description Return the pre-context log of a task. Use this to find
    ##'   out what has happened to a task that has unexpectedly failed, but
    ##'   for which `$log()` is uninformative.
    ##'
    ##' @param task_id A single task id to check
    dide_log = function(task_id) {
      dide_id <- self$dide_id(task_id)
      assert_scalar_character(dide_id, "task_id")
      cluster <- self$context$db$get(task_get_id(task_id), "dide_cluster")
      self$client$log(dide_id, cluster)
    },

    ##' @description Provision your context for running on the cluster.
    ##'   This sets up the remote set of packages that your tasks will use.
    ##'   See `vignette("packages")` for more information.
    ##'
    ##' @param policy The installation policy to use, as interpreted by
    ##'   `pkgdepends::pkg_solution` - so this should be `verylazy`/`lazy`
    ##'   (install missing packages but don't upgrade unless needed) or
    ##'   `upgrade` (upgrade packages as possible). In addition you can
    ##'   also use `later` which does nothing, or `fake` which pretends
    ##'   that it ran the provisioning. See `vignette("packages")` for
    ##'   details on these options.
    ##'
    ##' @param dryrun Do a dry run installation locally - this just
    ##'   checks that your requested set of packages is plausible, but does
    ##'   this without submitting a cluster job so it may be faster.
    ##'
    ##' @param quiet Logical, controls printing of informative messages
    ##'
    ##' @param show_progress Logical, controls printing of a spinning progress
    ##'   bar
    ##'
    ##' @param show_log Logical, controls printing of the log from the cluster
    provision_context = function(policy = "verylazy", dryrun = FALSE,
                                 quiet = FALSE, show_progress = NULL,
                                 show_log = TRUE) {
      policy <- provision_policy(policy)
      if (policy == "later") {
        ## Does not update the value of "provisioned", but any other
        ## successful exit will.
        return()
      }

      if (policy == "fake") {
        if (!quiet) {
          message("Assuming that everything is ok (policy = 'fake')")
        }
        run_provision <- FALSE
      } else {
        needs_rrq <- self$config$use_rrq || self$config$use_workers
        dat <- context_packages(self$context, needs_rrq)
        status <- private$lib$check(dat$packages)
        packages <- dat$packages
        run_provision <- TRUE
        if (policy == "verylazy") {
          policy <- "lazy"
          run_provision <- !status$complete
          packages <- status$missing
          if (!run_provision && !quiet) {
            message("Nothing to install; try running with policy = 'upgrade'")
          }
        }
      }

      if (run_provision) {
        self$install_packages(packages, dat$repos, policy, dryrun,
                              show_progress, show_log)
      }

      private$provisioned <- TRUE
    },

    ##' @description Install packages on the cluster. This can be used to
    ##'   more directly install packages on the cluster than the
    ##'   `$provision_context` method that you would typically use.
    ##'    See `vignette("packages")` for more information.
    ##'
    ##' @param packages A character vector of packages to install. These
    ##'   can be names of CRAN packages or GitHub references etc; see
    ##'   [pkgdepends::new_pkg_installation_proposal()] and
    ##'   `vignette("packages")` for more details
    ##'
    ##' @param repos A character vector of repositories to use when
    ##'   installing. A suitable CRAN repo will be added if not detected.
    ##'
    ##' @param policy The installation policy to use, as interpreted by
    ##'   `pkgdepends::pkg_solution` - so this should be `lazy`
    ##'   (install missing packages but don't upgrade unless needed) or
    ##'   `upgrade` (upgrade packages as possible). In addition you can
    ##'   also use `later` which does nothing, or `fake` which pretends
    ##'   that it ran the provisioning. See `vignette("packages")` for
    ##'   details on these options.
    ##'
    ##' @param dryrun Do a dry run installation locally - this just
    ##'   checks that your requested set of packages is plausible, but does
    ##'   this without submitting a cluster job so it may be faster.
    ##'
    ##' @param show_progress Logical, controls printing of a spinning progress
    ##'   bar
    ##'
    ##' @param show_log Logical, controls printing of the log from the cluster
    install_packages = function(packages, repos = NULL,
                                policy = "lazy", dryrun = FALSE,
                                show_progress = NULL, show_log = TRUE) {
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


submit_dide <- function(obj, data, task_ids, names, depends_on) {
  db <- obj$context$db
  config <- obj$config
  client <- obj$client
  batch_template <- data$templates$runner

  names <- setNames(task_names(task_ids, names), task_ids)

  ## Will be shared across all jobs submitted
  cluster <- config$cluster
  job_template <- config$resource$template
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
    deps <- self$dide_id(depends_on)
    deps <- ifelse(length(deps) > 0, "", paste0(deps, collapse = ","))
    dide_id <- client$submit(path, names[[id]], job_template, cluster,
                             resource_type, resource_count, deps)
    db$set(id, dide_id, "dide_id")
    db$set(id, config$cluster, "dide_cluster")
    db$set(id, path_logs(NULL), "log_path")
  }
}

unsubmit_dide <- function(obj, task_ids) {
  task_ids <- task_get_ids(task_ids)
  
  db <- obj$context$db
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

task_get_ids <- function(task_ids) {
  if (is.list(task_ids)) {
    vcapply(task_ids, task_get_ids)
  } else {
    task_get_id(task_ids)
  }
}


## What packages do we need?
context_packages <- function(context, need_rrq = FALSE) {
  ## 'callr' is needed if using 'rrq' because the rrq/context queue
  ## runs things in separate processes using callr, but this is only
  ## an optional dependency.
  packages <- setdiff(unique(c("context",
                               if (need_rrq) c("rrq", "callr"),
                               context$packages$attached,
                               context$packages$loaded,
                               context$package_sources$packages)),
                      builtin_packages())
  repos <- c(context$package_sources$repos,
             didehpc = "https://mrc-ide.github.io/didehpc-pkgs")
  list(packages = packages,
       repos = repos)
}


builtin_packages <- function() {
  rownames(utils::installed.packages(priority = "high"))
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
