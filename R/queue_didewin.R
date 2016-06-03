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
##' @param rtools Indicates if Rtools is required (needed in cases
##'   where you need a C/C++ compiler, such as using Rcpp's inline
##'   support, or rstan).  The default will try to detect if it is
##'   required.
##'
##' @export
queue_didewin <- function(context, config=didewin_config(), initialise=TRUE,
                          rtools=NULL, sync=NULL) {
  .R6_queue_didewin$new(context, config, initialise, rtools, sync)
}

.R6_queue_didewin <- R6::R6Class(
  "queue_didewin",
  inherit=queuer:::.R6_queue_base,
  public=list(
    config=NULL,
    logged_in=FALSE,
    sync=NULL,
    templates=NULL,

    initialize=function(context, config, initialise, rtools, sync) {
      if (!inherits(config, "didewin_config")) {
        stop("Expected a didewin_config for 'config'")
      }
      super$initialize(context, initialise)

      if (isTRUE(config$use_rrq_workers)) {
        ## TODO: This is annoying because it means that all workers
        ## will share a key across multiple invocations.  So this may
        ## change in future.  Probably this should happen in the
        ## config bit but that requires fixing that so it knows about
        ## workers.
        config$rrq_key_alive <- rrq::rrq_key_worker_alive(context$id)
      }
      self$config <- config

      ## Will throw if the context is not network accessible.
      prepare_path(context::context_root(context), config$shares)

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

      dir.create(path_batch(context$root), FALSE, TRUE)
      dir.create(path_logs(context$root), FALSE, TRUE)

      if (use_hpctools(self$config)) {
        self$logged_in <- TRUE
      } else if (initialise) {
        self$login()
      }

      if (initialise) {
        self$sync_files()
      }

      initialise_windows_packages(self)

      if (needs_rtools(rtools, self$config, self$context)) {
        self$config$rtools <- rtools_info(self$config)
      }

      workdir <- self$config$workdir %||% self$workdir
      self$templates <- batch_templates(context, config, workdir)
    },

    login=function(always=TRUE) {
      if (always || !self$logged_in) {
        web_login(self$config)
        self$logged_in <- TRUE
      }
    },

    sync_files=function(verbose=TRUE, delete=TRUE) {
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
        syncr::syncr(self$sync, dest, verbose=verbose, delete=delete)
      }
    },

    ## I don't think that this is wise.  Better would be a function
    ## for *generally* updating the configuration.  But in general
    ## we're better off just making a new object probably.
    set_cluster=function(cluster=NULL) {
      if (is.null(cluster)) {
        cluster <- setdiff(valid_clusters(), self$config$cluster)
      } else {
        cluster <- match_value(cluster, valid_clusters())
      }
      self$config$cluster <- cluster
    },

    cluster_load=function(cluster=NULL, nodes=TRUE) {
      self$login(FALSE)
      print(didewin_shownodes(self$config, cluster %||% self$config$cluster),
            nodes=nodes)
    },

    tasks_status_dide=function(task_ids=NULL) {
      self$login(FALSE)
      check_tasks_status_dide(self, task_ids)
    },

    submit=function(task_ids, names=NULL) {
      self$login(FALSE)
      ## See below:
      submit(self, task_ids, names)
    },
    unsubmit=function(t) {
      self$login(FALSE)
      ## TODO: The task_get_id functionality would be nice throughout
      ## the class, probably at the base level.  But need to be
      ## careful to get it really consistent or it will just be
      ## confusing.
      unsubmit(self, task_get_id(t))
    },
    submit_workers=function(n, rrq, wait=FALSE) {
      self$login(FALSE)
      submit_workers(self, n, rrq, wait)
    },

    dide_id=function(t) {
      task_ids <- task_get_id(t, self)
      db <- context::context_db(self)
      setNames(vcapply(task_ids, db$get, "dide_id"), names(task_ids))
    },

    dide_log=function(t) {
      self$login(FALSE)
      dide_task_id <- self$dide_id(t)
      assert_scalar_character(dide_task_id, "task_id") # bit of trickery
      didewin_joblog(self$config, dide_task_id)
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
initialise_windows_packages <- function(obj) {
  context <- obj$context
  build_server <- obj$config$build_server

  path_lib <- file.path(context$root, "R", R_PLATFORM, R_VERSION)

  ## TODO: look to see if any of the packages in the local drat need
  ## compilation.  If so we might be best trying for an on-cluster
  ## installation.
  r_version_2 <- as.character(R_VERSION[1, 1:2]) # used for talking to CRAN
  context::cross_install_context(path_lib, "windows", r_version_2, context)

  ## TODO: These bits are really not very nice!
  if (isTRUE(obj$config$use_workers)) {
    ## TODO: Not in the right place but keeping this stuff together
    ## for now.
    dir.create(path_worker_logs(context$root), FALSE, TRUE)
    repos <- c(CRAN="https://cran.rstudio.com",
               richfitz="https://richfitz.github.io/drat/")
    context::cross_install_packages(
      path_lib, "windows", r_version_2, repos, c("queuer", "seagull"))
  }

  if (isTRUE(obj$config$use_rrq_workers)) {
    loadNamespace("rrq") # force presence of these packages
    dir.create(path_rrq_worker_logs(context$root), FALSE, TRUE)
    repos <- c(CRAN="https://cran.rstudio.com",
               richfitz="https://richfitz.github.io/drat/")
    context::cross_install_packages(
      path_lib, "windows", r_version_2, repos, "rrq")
    ## TODO: once settled, make optional.
    dest <- file.path(context::context_root(obj), "bin", "rrq_worker")
    file.copy(system.file("rrq_worker_bootstrap", package="rrq"), dest)
  }
}

initialise_packages_on_cluster <- function(obj, timeout=Inf) {
  t <- obj$enqueue_(quote(sessionInfo()))
  message("Initialising packages on the cluster itself")
  message("You'll need to check the status of the job if this doesn't complete")
  ## TODO: This one should poll the actual job status of the task on
  ## the cluster rather than the result in context because the failure
  ## will not be correctly logged.  I should actually tweak that I
  ## think; once we have context loaded there's no reason why a job
  ## cannot error.  This would be really good and not too bad to test.
  t$wait(timeout=timeout)
}

check_binary_packages <- function(db, path_drat) {
  fields <- c("Package", "Version", "MD5sum", "NeedsCompilation")
  path_src <- file.path(path_drat, "src/contrib")
  pkgs <- as.data.frame(read.dcf(file.path(path_src, "PACKAGES"),
                                 fields=fields),
                        stringsAsFactors=FALSE)
  i <- pkgs$NeedsCompilation == "yes"
  if (any(i)) {
    pkgs <- pkgs[i, , drop=FALSE]
  } else {
    return(list(packages=character(0),
                packages_source=character(0),
                hash=character(0)))
  }

  ## At this point, check the appropriate binary directory.  That's
  ## going to depend on the target R version (which will be the same
  ## as the build server ideally).
  r_version_2 <- paste(unclass(R_VERSION)[[1]][1:2], collapse=".")
  path_bin <- file.path(path_drat, "bin/windows/contrib", r_version_2)

  ## This is no longer sufficient because we'd want to check to see if
  ## binaries had been added anyway...
  if (file.exists(path_bin)) {
    f <- function(hash) {
      bin <- tryCatch(db$get(hash, "binary"), KeyError=function(e) NULL)
      if (is.null(bin)) {
        TRUE
      } else {
        dest <- file.path(path_bin, names(bin))
        !file.exists(dest) || unname(tools::md5sum(dest)) != bin[[1]]
      }
    }
    rebuild <- vlapply(pkgs$MD5sum, f)

    pkgs <- pkgs[rebuild, , drop=FALSE]
  }

  packages_source <-
    file.path(path_src, sprintf("%s_%s.tar.gz", pkgs$Package, pkgs$Version))
  list(packages=pkgs$Package,
       packages_source=packages_source,
       hash=pkgs$MD5sum,
       dest=path_bin)
}

## TODO: rework queuer to pass names as the *names* of task_ids which
## is way simpler.
submit <- function(obj, task_ids, names) {
  if (isTRUE(obj$config$use_workers)) {
    ## TODO: unexported function.
    queuer:::queue_local_submit(obj, task_ids)
  } else {
    submit_dide(obj, task_ids, names)
  }
}

submit_dide <- function(obj, task_ids, names) {
  db <- context::context_db(obj)
  root <- context::context_root(obj)
  config <- obj$config
  workdir <- obj$config$workdir %||% obj$workdir
  template <- obj$templates$runner

  pb <- progress::progress_bar$new("Submitting [:bar] :current / :total",
                                   total=length(task_ids))

  if (is.null(names)) {
    names <- setNames(task_ids, task_ids)
  } else if (length(names) == length(task_ids)) {
    names <- setNames(sprintf("%s (%s)", names, task_ids), task_ids)
  } else {
    stop("incorrect length names")
  }

  ## TODO: in theory this can be done in bulk on the cluster but it
  ## requires some support on the web interface I think.
  for (id in task_ids) {
    batch <- write_batch(id, root, template)
    path <- remote_path(prepare_path(batch, config$shares))
    pb$tick()
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
    queuer:::queue_local_unsubmit(obj, task_ids)
  } else {
    unsubmit_dide(obj, task_ids)
  }
}

unsubmit_dide <- function(obj, task_ids) {
  db <- context::context_db(obj)
  dide_id <- vcapply(task_ids, db$get, "dide_id")
  dide_cluster <- vcapply(task_ids, db$get, "dide_cluster")
  config <- obj$config

  pb <- progress::progress_bar$new("Cancelling [:bar] :current / :total",
                                   total=length(task_ids))
  ret <- character(length(task_ids))
  for (i in seq_along(task_ids)) {
    pb$tick()
    id <- task_ids[[i]]
    st <- tryCatch(db$get(id, "task_status"),
                   KeyError=function(e) NULL)
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

submit_workers <- function(obj, n, rrq=FALSE, wait=FALSE) {
  db <- context::context_db(obj)
  root <- context::context_root(obj)
  config <- obj$config
  workdir <- obj$config$workdir %||% obj$workdir
  id <- obj$context$id
  template <- if (rrq) obj$templates$rrq_worker else obj$templates$worker

  pb <- progress::progress_bar$new("Submitting [:bar] :current / :total",
                                   total=n)
  names <- paste0(ids::adjective_animal(), "_", seq_len(n))

  ## TODO: it would be good if the workers could self-register on
  ## startup so that we know if they're still up.  Otherwise this
  ## going to be a bit of a trick.
  ##
  ## The Redis workers can, but it's a bit of a faff.  Realistically,
  ## this does not need to be done though because we can start while
  ## we wait for the workers to come up.
  path_log <- if (rrq) path_rrq_worker_logs(NULL) else path_worker_logs(NULL)

  ## TODO: I might have enough stuff here that I need another cluster
  ## object.
  for (nm in names) {
    batch <- write_batch(nm, root, template, FALSE)
    path <- remote_path(prepare_path(batch, config$shares))
    pb$tick()
    dide_id <- didewin_submit(config, path, nm)
    didewin_joblog(config, dide_id)
    ## NOTE: there is nothing here to organise the interaction with
    ## these yet, though some things might work directly.
    db$set(nm, dide_id,        "dide_id")
    db$set(nm, config$cluster, "dide_cluster")
    db$set(id, path_log,       "log_path")
  }

  if (wait && rrq) {
    con <- redux::hiredis(host=obj$config$cluster)
    rrq::workers_wait(con, n, obj$config$rrq_key_alive)
  }
  names
}

## What we're really looking for here is:
##  ctx      dide
##  PENDING  RUNNING -> setup, possibly stalled -> update to RUNNING
##  PENDING  ERROR   -> setup, has failed       -> update to ERROR
##  PENDING  CANCELLED -> setup, manually cancelled -> update to CANCELLED
##  RUNNING  ERROR   -> failure that we can't catch -> update to ERROR
##  RUNNING  COMPLETE -> probable failure that has not been caught -> ERROR
##  RUNNING  CANCELLED -> was running, manually cancelled -> update to CANCELLED
check_tasks_status_dide <- function(obj, task_ids=NULL) {
  if (is.null(task_ids)) {
    task_ids <- obj$tasks_list()
  }
  st_ctx <- obj$tasks_status(task_ids)
  db <- context::context_db(obj)

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
         paste(task_ids[is.na(i)], collapse=", "))
  }

  ok <- TRUE
  st_dide <- dat$status[i]

  i <- st_dide == "ERROR"
  if (any(i)) {
    j <- i & st_ctx == "PENDING"
    if (any(j)) {
      message("Tasks have failed while context booting:\n",
              paste(sprintf("\t- %s", task_ids[j]), collapse="\n"))
    }
    j <- i & st_ctx == "RUNNING"
    if (any(j)) {
      message("Tasks have crashed after starting:\n",
              paste(sprintf("\t- %s", task_ids[j]), collapse="\n"))
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
              paste(sprintf("\t- %s", task_ids[i][res]), collapse="\n"))
    }
    ok <- FALSE
  }

  i <- st_dide == "CANCELLED"
  if (any(i)) {
    j <- i & st_ctx == "PENDING"
    if (any(j)) {
      message("Tasks cancelled while context booting:\n",
              paste(sprintf("\t- %s", task_ids[j]), collapse="\n"))
    }
    j <- i & st_ctx == "RUNNING"
    if (any(j)) {
      message("Tasks cancelled after starting:\n",
              paste(sprintf("\t- %s", task_ids[j]), collapse="\n"))
    }
    lapply(task_ids[i], set_task_error)
    ok <- FALSE
  }

  i <- st_ctx == "PENDING" & st_dide == "RUNNING"
  if (any(i)) {
    message("Tasks have started on cluster, but context still booting:\n",
            paste(sprintf("\t- %s", task_ids[i]), collapse="\n"))
    ok <- FALSE
  }

  if (ok) {
    message("Reported statuses seem accurate to me")
  }
  invisible(NULL)
}

check_rsync <- function(config) {
  withCallingHandlers(
    loadNamespace("syncr"),
    error=function(e)
      "See https://dide-tools.github.io/didewin for syncr installation")
  if (!syncr::has_rsync()) {
    if (is_windows()) {
      path_rsync <- file.path(rtools_info(config)$path, "bin", "rsync")
      if (file.exists(path_rsync)) {
        options("syncr.rsync"=path_rsync)
        if (syncr::has_rsync()) {
          return()
        }
      }
    }
    stop("Could not find rsync binary; can't run out of place")
  }
}

## A helper function that will probably move into queue_base
task_get_id <- function(x, obj=NULL) {
  if (inherits(x, "task")) {
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
