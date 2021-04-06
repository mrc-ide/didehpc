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
  login <- provision <- initialise
  .R6_queue_didehpc$new(context, config, root, login, provision)
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

    initialize = function(context, config, root, login, provision) {
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

      if (login) {
        self$login()
      }
      if (provision) {
        self$provision_context("skip")
      }
    },

    login = function(refresh = TRUE) {
      self$client$login(refresh)
    },

    cluster_load = function(cluster = NULL, nodes = TRUE) {
      if (isTRUE(cluster)) {
        print(self$client$load_overall())
      } else {
        print(self$client$load_node(cluster %||% self$config$cluster),
              nodes = nodes)
      }
    },

    task_status_dide = function(task_ids = NULL) {
      self$login(FALSE)
      task_status_dide(self, task_ids)
    },

    submit = function(task_ids, names = NULL) {
      if (!private$provisioned) {
        self$provision_context("skip")
      }
      submit_dide(obj, task_ids, names)
    },

    unsubmit = function(t) {
      unsubmit_dide(obj, task_ids)
    },

    dide_id = function(t) {
      task_ids <- task_get_id(t, self)
      db <- self$db
      db$mget(task_ids, "dide_id")
      setNames(vcapply(task_ids, db$get, "dide_id"), names(task_ids))
    },

    dide_log = function(t) {
      dide_task_id <- self$dide_id(t)
      assert_scalar_character(dide_task_id, "task_id")
      cluster <- self$db$get(task_get_id(t), "dide_cluster")
      self$client$log(dide_id, cluster)
    },

    provision_context = function(policy = "skip", dryrun = FALSE) {
      need_rrq <- self$config$use_rrq || self$config$use_workers
      dat <- context_packages(self$context, need_rrq)
      self$install_packages(dat$packages, dat$repos, policy, dryrun)
      private$provisioned <- TRUE
    },

    install_packages = function(packages, repos = NULL,
                                policy = "skip", dryrun = FALSE) {
      complete <- private$lib$check(packages)$complete
      if (complete && policy == "skip") {
        return()
      }
      private$lib$provision(packages, repos, policy, dryrun)
    }
  ),

  private = list(
    lib = NULL,
    provisioned = NULL
  ))


submit_dide <- function(obj, task_ids, names) {
  db <- obj$db
  root <- obj$context$root$path
  config <- obj$config
  client <- obj$client
  batch_template <- obj$templates$runner

  if (is.null(names)) {
    names <- setNames(task_ids, task_ids)
  } else if (length(names) == length(task_ids)) {
    names <- setNames(sprintf("%s (%s)", names, task_ids), task_ids)
  } else {
    stop("incorrect length names")
  }

  ## Will be shared across all jobs submitted
  template <- config$template
  cluster <- config$cluster
  resource_type <- config$resource$type
  resource_count <- config$resource$count

  ## TODO: in theory this can be done in bulk on the cluster but it
  ## requires some support on the web interface as we do not get
  ## access to multiple names via the form.
  p <- queuer::progress_timeout(length(task_ids), Inf, label = "submitting ")
  for (id in task_ids) {
    batch <- write_batch(id, root, batch_template, list(task_id = id))
    path <- remote_path(batch, config$shares)
    p()
    dide_id <- client$submit(path, names[[id]], template, cluster,
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
