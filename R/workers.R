initialise_rrq <- function(obj) {
  if (isTRUE(obj$config$use_rrq) || isTRUE(obj$config$use_workers)) {
    loadNamespace("rrq")
    dir.create(path_worker_logs(obj$root$path), FALSE, TRUE)

    if (is.null(obj$context$unique_value)) {
      stop("context must contain a unique_value; please rerun context_save()")
    }

    config <- obj$config
    rrq:::write_rrq_worker(obj$context)
    rrq:::worker_config_save(obj$context,
                             config$cluster,
                             redis_host = redis_host(config$cluster),
                             redis_port = 6379,
                             timeout = config$worker_timeout,
                             log_path = path_logs(NULL))

    ## Both approaches use workers:
    initialise_rrq_controllers(obj)
  }
}

## This is here because we might need to rerun it periodically and I
## don't want to have to redo the installation....
initialise_rrq_controllers <- function(obj) {
  con <- rrq_redis_con(config)
  if (isTRUE(obj$config$use_workers)) {
    obj$workers <- rrq::worker_controller(obj$context$id, con)
  }
  if (isTRUE(obj$config$use_rrq)) {
    obj$rrq <- rrq::rrq_controller(obj$context, con)
  }
}

submit_workers <- function(obj, n, timeout = 600, progress = TRUE) {
  if (!(isTRUE(obj$config$use_rrq) || isTRUE(obj$config$use_workers))) {
    stop("workers not enabled")
  }

  db <- obj$context$db
  root <- obj$root$path
  config <- obj$config
  template <- obj$templates$rrq_worker

  path_log <- path_worker_logs(NULL)
  linux <- linux_cluster(config$cluster)

  names <- sprintf("%s_%d", ids::adjective_animal(), seq_len(n))
  rrq_key_alive <- rrq::rrq_expect_workers(obj, names)

  message(sprintf("Submitting %d %s", n, ngettext(n, "worker", "workers")))
  pb <- queuer:::progress(n, show = progress)

  ## It would seem that it should be possible to bulk submit here, but
  ## that's not straightforward because the php script can then take
  ## ages (because the underlying HPC programs are slow).  So bulk
  ## submission would need to run in batch and would be pretty error
  ## prone.  At some point I may set up a batched bulk submission
  ## here, but it's not actually going to make things much faster
  ## because the job submission part is the bottleneck.
  dat <- list(rrq_worker_id = NULL,
              rrq_key_alive = rrq_key_alive)
  for (nm in names) {
    dat$rrq_worker_id <- nm
    batch <- write_batch(nm, root, template, dat, linux)
    path <- remote_path(prepare_path(batch, config$shares))
    pb()
    dide_id <- didewin_submit(config, path, nm)
    ## didewin_joblog(config, dide_id)
    ## NOTE: there is nothing here to organise the interaction with
    ## these yet, though some things might work directly.
    db$mset(nm,
            c(dide_id,   path_log,   config$cluster),
            c("dide_id", "log_path", "dide_cluster"))
  }

  rrq::workers_wait(rrq_redis_con(obj$config), n, rrq_key_alive,
                    timeout = timeout, time_poll = 1, progress = progress)
}

rrq_redis_con <- function(config) {
  redux::hiredis(host=paste0(config$cluster, ".dide.ic.ac.uk"))
}
