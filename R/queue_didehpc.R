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
  inherit = queuer:::R6_queue_base,
  public = list(
    config = NULL,
    client = NULL,

    initialize = function(context, config, root, initialise, client = NULL) {
      super$initialize(context, root, initialise)
      self$config <- as_didehpc_config(config)

      path_root <- self$context$root$path
      private$data <- batch_data(path_root, self$context$id, self$config)

      if (is.null(client)) {
        client <- web_client$new(self$config$credentials,
                                 self$config$cluster,
                                 initialise)
      }
      self$client <- client

      private$lib <- queue_library$new(
        private$data, self$config$cluster, self$client)

      if (self$config$use_workers || self$config$use_rrq) {
        rrq_init(self$rrq_controller(), self$config)
      }

      if (initialise) {
        self$provision_context("lazy")
      }
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
      submit_dide(self, private$data, task_ids, names)
    },

    submit_workers = function(n, timeout = 600, progress = NULL) {
      submit_workers(self, private$data, n, timeout, progress)
    },

    stop_workers = function(worker_ids = NULL) {
      self$rrq_controller()$worker_stop(worker_ids)
    },

    rrq_controller = function() {
      check_rrq_enabled(self$config)
      host <- rrq_redis_host(self$config$cluster)
      rrq::rrq_controller(self$context$id, redux::hiredis(host = host))
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

    provision_context = function(policy = "lazy", dryrun = FALSE) {
      dat <- context_packages(self$context,
                              self$config$use_rrq || self$config$use_workers)
      self$install_packages(dat$packages, dat$repos, policy, dryrun)
      private$provisioned <- TRUE
    },

    install_packages = function(packages, repos = NULL,
                                policy = "lazy", dryrun = FALSE) {
      complete <- private$lib$check(packages)$complete
      if (complete && policy == "lazy") {
        message("Nothing to install; try running with policy = 'upgrade'")
        return()
      }
      message("Running installation script on cluster")
      private$lib$provision(packages, repos, policy, dryrun)
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
  root <- obj$context$root$path
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
  list(packages = unique(c("context",
                           if (need_rrq) "rrq",
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
