initialise_rrq <- function(obj) {
  if (isTRUE(obj$config$use_rrq) || isTRUE(obj$config$use_workers)) {
    loadNamespace("rrq")
    root <- context::context_root(obj)
    ## TODO: This is annoying because it means that all workers
    ## will share a key across multiple invocations.  So this may
    ## change in future.  Probably this should happen in the
    ## config bit but that requires fixing that so it knows about
    ## workers.
    obj$config$rrq_key_alive <- rrq::rrq_key_worker_alive(obj$context$id)
    loadNamespace("rrq") # force presence of these packages
    dir.create(path_worker_logs(root), FALSE, TRUE)
    repos <- c(CRAN="https://cran.rstudio.com",
               richfitz="https://richfitz.github.io/drat/")
    platform <- r_platform(obj$config$cluster)
    path_lib <- file.path(root, "R", platform, R_VERSION)
    ## TODO: duplicated all over the show:
    ## TODO: This is not going to work on Linux
    r_version_2 <- as.character(R_VERSION[1, 1:2]) # used for talking to CRAN
    ## TODO: use a wrapper
    context::cross_install_packages(
      path_lib, "windows", r_version_2, repos, c("rrq", "redux"))
    dest <- file.path(root, "bin", "rrq_worker")
    file.copy(system.file("rrq_worker_bootstrap", package="rrq"), dest)
    initialise_rrq_controllers(obj)

    if (is.null(context::context_read(obj$context)$unique_value)) {
      message(
        "I recommend saving a unique_value into your context for use with rrq")
    }
  }
}

## This is here because we might need to rerun it periodically and I
## don't want to have to redo the installation....
initialise_rrq_controllers <- function(obj) {
  if (isTRUE(obj$config$use_workers)) {
    con <- rrq_redis_con(obj$config)
    obj$workers <- rrq::worker_controller(obj$context$id, con)
  }
  if (isTRUE(obj$config$use_rrq)) {
    con <- rrq_redis_con(obj$config)
    obj$rrq <- rrq::rrq_controller(obj$context, con, obj$context_envir)
  }
}

## TODO: we need to make sure that it's easy to set the "no job"
## timeout.  I think that ~10 minutes is a reasonable amount of time,
## but whatever it is, it needs to be configurable easily.
## Unfortunately `worker_spawn` doesn't even take this as an option,
## so that needs adding (it didn't exist in the queue_local
## arrangement).
submit_workers <- function(obj, n, wait=TRUE) {
  if (!(isTRUE(obj$config$use_rrq) || isTRUE(obj$config$use_workers))) {
    stop("workers not enabled")
  }

  db <- context::context_db(obj)
  root <- context::context_root(obj)
  config <- obj$config
  workdir <- obj$config$workdir %||% obj$workdir
  id <- obj$context$id
  template <- obj$templates$rrq_worker

  pb <- progress::progress_bar$new("Submitting [:bar] :current / :total",
                                   total=n)
  names <- paste0(ids::adjective_animal(), "_", seq_len(n))
  path_log <- path_worker_logs(NULL)

  ## It would seem that it should be possible to bulk submit here, but
  ## that's not straightforward because the php script can then take
  ## ages (because the underlying HPC programs are slow).  So bulk
  ## submission would need to run in batch and would be pretty error
  ## prone.  At some point I may set up a batched bulk submission
  ## here, but it's not actually going to make things much faster
  ## because the job submission part is the bottleneck.
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
    db$set(nm, path_log,       "log_path")
  }

  ## TODO: The logic here is really nasty.
  if (wait) {
    rrq::workers_wait(rrq_redis_con(obj$config), n, obj$config$rrq_key_alive)
  }
  names
}

rrq_redis_con <- function(config) {
  redux::hiredis(host=paste0(config$cluster, ".dide.ic.ac.uk"))
}
