##' Create a queue object
##'
##' Queues are R6 objects with many methods.  They need documenting still.
##' @title Create a queue object
##' @param context A context
##' @param config Optional dide configuration information.
##' @param login log in to the cluster on queue creation (recommended)
##' @export
queue_didewin <- function(context, config=didewin_config(), login=TRUE) {
  .R6_queue_didewin$new(context, config, login)
}

.R6_queue_didewin <- R6::R6Class(
  "queue_didewin",
  inherit=queuer:::.R6_queue_base,
  public=list(
    config=NULL,
    initialize=function(context, config, login) {
      super$initialize(context)
      self$config <- config

      dir.create(path_batch(context$root), FALSE, TRUE)
      dir.create(path_logs(context$root), FALSE, TRUE)
      ## Not sure about these two:
      ##   dir.create(path_dide_task_id(context$root), FALSE, TRUE)
      ##   dir.create(path_dide_cluster(context$root), FALSE, TRUE)
      ## -- instead, do this via submit()

      if (login) {
        self$login()
      }

      initialise_packages(self)
    },

    login=function() {
      web_login(self$config)
    },
    set_cluster=function(cluster=NULL) {
      if (is.null(cluster)) {
        cluster <- setdiff(valid_clusters(), self$config$cluster)
      } else {
        cluster <- match_value(cluster, valid_clusters())
      }
      self$config$cluster <- cluster
    },

    cluster_load=function(cluster=NULL) {
      web_shownodes(if (is.null(cluster)) self$config$cluster else cluster)
    },

    tasks_status_dide=function(task_ids=NULL) {
      tasks_status_dide(self, task_ids)
    },

    submit=function(task_ids) {
      ## See below:
      submit(self, task_ids)
    },
    unsubmit=function(task_ids) {
      unsubmit(self, task_ids)
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
initialise_packages <- function(obj) {
  context <- obj$context
  build_server <- obj$config$build_server

  path_lib <- file.path(context$root, "R", R_PLATFORM, R_VERSION)
  packages <- c("context", unlist(context$packages, use.names=FALSE))
  if (all(packages %in% .packages(TRUE, path_lib))) {
    return()
  }

  ## Next, look to see if any of the packages in the local drat need
  ## compilation.
  r_version_2 <- as.character(R_VERSION[1, 1:2]) # used for talking to CRAN
  context::cross_install_context(path_lib, "windows", r_version_2, context)
}

initialise_packages_on_cluster <- function(obj) {
  t <- obj$enqueue_(quote(sessionInfo()))
  message("Initialising packages on the cluster itself")
  message("You'll need to check the status of the job if this doesn't complete")
  t$wait(timeout=600)
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


submit <- function(obj, task_ids) {
  db <- context::context_db(obj)
  root <- obj$context$root
  config <- obj$config

  ## TODO: in theory this can be done in bulk on the cluster but it
  ## requires some support on the web interface I think.
  for (id in task_ids) {
    batch <- write_batch(root, id, config, obj$workdir)
    path <- remote_path(prepare_path(batch, config$shares))
    dide_id <- web_submit(path, config, id)
    db$set(id, dide_id,        "dide_id")
    db$set(id, config$cluster, "dide_cluster")
  }
}

unsubmit <- function(obj, task_ids) {
  db <- context::context_db(obj)
  dide_id <- vcapply(task_ids, db$get, "dide_id")
  dide_cluster <- vcapply(task_ids, db$get, "dide_cluster")

  ## It's possible this is vectorised.
  ret <- vector("list", length(task_ids))
  for (i in seq_along(task_ids)) {
    id <- task_ids[[i]]
    dide_id <- db$get(id, "dide_id")
    cluster <- db$get(id, "dide_cluster")
    ret[[i]] <- web_cancel(cluster, dide_id)
  }
  ret
}

## What we're really looking for here is:
##  ctx      dide
##  PENDING  RUNNING -> setup, possibly stalled -> update to RUNNING
##  PENDING  ERROR   -> setup, has failed       -> update to ERROR
##  RUNNING  ERROR   -> failure that we can't catch -> update to ERROR
##  RUNNING  COMPLETE -> probable failure that has not been caught -> ERROR
tasks_status_dide <- function(obj, task_ids=NULL) {
  if (is.null(task_ids)) {
    task_ids <- obj$tasks_list()
  }
  st_ctx <- obj$tasks_status(task_ids)
  db <- context::context_db(obj)

  i <- st_ctx %in% c("PENDING", "RUNNING")
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

  ## Realistically we're not interested in Finished here, and that does
  ## bank up after a bit.  Talk with Wes about improvements perhaps?
  dat <- web_jobstatus(obj$config$username, obj$config$cluster)
  i <- match(task_ids, dat$name)
  if (any(is.na(i))) {
    stop("Did not find information on tasks: ",
         paste(tasks[is.na(i)], collapse=", "))
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
