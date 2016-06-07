initialise_seagull <- function(obj) {
  if (isTRUE(obj$config$use_workers)) {
    loadNamespace("seagull")
    root <- context::context_root(obj)
    dir.create(path_worker_logs(context$root), FALSE, TRUE)
    repos <- c(CRAN="https://cran.rstudio.com",
               richfitz="https://richfitz.github.io/drat/")
    path_lib <- file.path(root, "R", R_PLATFORM, R_VERSION)
    ## TODO: duplicated all over the show:
    r_version_2 <- as.character(R_VERSION[1, 1:2]) # used for talking to CRAN
    context::cross_install_packages(
      path_lib, "windows", r_version_2, repos, c("queuer", "seagull"))
  }
}

submit_workers <- function(obj, n, wait=NULL) {
  if (isTRUE(obj$config$use_rrq)) {
    rrq <- TRUE
  } else if (isTRUE(obj$config$use_workers)) {
    rrq <- FALSE
  } else {
    stop("workers not enabled")
  }
  wait <- wait %||% rrq
  if (wait && !rrq) {
    ## TODO: it would be good if the workers could self-register on
    ## startup so that we know if they're still up.  Otherwise this
    ## going to be a bit of a trick.  However, given that these are
    ## submitted after the tasks and the tasks are the main aim it's
    ## less useful to know this.
    stop("Waiting for seagull workers is not supported")
  }

  db <- context::context_db(obj)
  root <- context::context_root(obj)
  config <- obj$config
  workdir <- obj$config$workdir %||% obj$workdir
  id <- obj$context$id
  template <- if (rrq) obj$templates$rrq_worker else obj$templates$worker

  pb <- progress::progress_bar$new("Submitting [:bar] :current / :total",
                                   total=n)
  names <- paste0(ids::adjective_animal(), "_", seq_len(n))

  path_log <- if (rrq) path_rrq_worker_logs(NULL) else path_worker_logs(NULL)

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

  ## TODO: The logic here is really nasty.
  if (wait && rrq) {
    con <- redux::hiredis(host=obj$config$cluster)
    rrq::workers_wait(con, n, obj$config$rrq_key_alive)
  }
  names
}
