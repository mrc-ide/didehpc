## Create and configure an rrq controller for use from the controlling
## node
didehpc_rrq_controller <- function(config, id) {
  if (!(isTRUE(config$use_rrq) || isTRUE(config$use_workers))) {
    stop("workers not enabled")
  }
  rrq::rrq_controller$new(id, redux::hiredis(host = config$redis_host))
}


## Basic initialisation of the rrq environment by setting up the
## environment creation and worker configuration.
rrq_init <- function(rrq, config) {
  ## All workers read the full context
  rrq$envir(rrq_context_loader(), notify = FALSE)
  ## NOTE: we should consider a heartbeat here
  ##
  ## NOTE: Jobs that use rrq controller *from* a job risk a
  ## deadlock because we could request more work than we have
  ## workers
  timeout_idle <- config$worker_timeout
  queue <- c("default", "context")
  cfg <- rrq::rrq_worker_config(timeout_idle = timeout_idle, queue = queue)
  rrq$worker_config_save("didehpc", cfg)
}


## Submit a set of context jobs into the redis queue
rrq_submit_context_tasks <- function(config, context, task_ids, names) {
  rrq <- didehpc_rrq_controller(config, context$id)
  ## NOTE: We don't cope with names here
  root <- context$root$path
  db <- context$root$db
  dat <- data.frame(task_id = task_ids,
                    path_log = path_logs(root, task_ids),
                    stringsAsFactors = FALSE)
  res <- rrq$enqueue_bulk_(dat, quote(context::task_run_external),
                           root = root,
                           identifier = context$id,
                           timeout_task_wait = 0)
  ## TODO: set something in as dide_cluster and dide_id here to
  ## prevent reconcile() marking these as dead. Given that things can
  ## end up on multiple clusters, I think that we might be better off
  ## marking the cluster as "rrq" rather than either of the fi-- names
  ## and later in reconcile we can check for that.
  ##
  ## TODO: here (and above) we have to use path_logs because the local
  ## log path includes the context root which we don't want.
  n <- length(task_ids)
  db$mset(task_ids, rep_len(path_logs(NULL), n), "log_path")
  db$mset(task_ids, rep_len("rrq", n), "dide_cluster")
}


## Used in the initialisation, will load the context on a remote
## machine based on environment variables.
rrq_context_loader <- function() {
  create <- function(envir) {
    context_root <- Sys.getenv("CONTEXT_ROOT")
    context_id <- Sys.getenv("CONTEXT_ID")
    if (context_root == "" | context_id == "") {
      stop(sprintf(
        "Environment variables incorrect: CONTEXT_ROOT: '%s' CONTEXT_ID: '%s')",
        context_root, context_id))
    }
    ctx <- context::context_read(context_id, context_root)
    context::context_load(ctx, envir)
  }
  environment(create) <- globalenv()
  create
}


rrq_submit_workers <- function(obj, data, n, timeout = 600,
                               progress = NULL) {
  config <- obj$config
  client <- obj$client
  batch_template <- data$templates$rrq_worker

  path_lib <- path_library(obj$context$root$path, config$r_version)
  rrq_check_package_version(
    utils::packageVersion("rrq"),
    utils::packageVersion("rrq", lib.loc = path_lib))

  ## Will be shared across all jobs submitted
  cluster <- config$cluster
  resource <- config$worker_resource %||% config$resource
  job_template <- resource$template
  resource_type <- resource$type
  resource_count <- resource$count

  base <- ids::adjective_animal()
  worker_ids <- sprintf("%s_%d", base, seq_len(n))

  rrq <- obj$rrq_controller()
  rrq_init(rrq, obj$config)

  rrq_key_alive <- rrq::rrq_worker_expect(rrq, worker_ids)

  message(sprintf("Submitting %d %s with base name '%s'",
                  n, ngettext(n, "worker", "workers"), base))
  p <- queuer::progress_timeout(n, timeout, label = "submitting ",
                                show = progress)

  dir.create(data$paths$local$batch, FALSE, TRUE)
  dir.create(data$paths$local$worker_log, FALSE, TRUE)
  rrq::rrq_worker_script(file.path(data$paths$local$root, "bin"))
  for (id in worker_ids) {
    dat <- list(rrq_worker_id = id)
    base <- paste0(id, ".bat")
    batch <- file.path(data$paths$local$batch, base)
    writeLines(glue_whisker(batch_template, dat), batch)
    path <- windows_path(file.path(data$paths$remote$batch, base))
    p()
    dide_id <- client$submit(path, id, job_template, cluster,
                             resource_type, resource_count)
  }

  ## We should also check here that the job is still running
  rrq::rrq_worker_wait(rrq, rrq_key_alive, timeout = timeout,
                       progress = progress)
}


rrq_stop_workers <- function(config, id, worker_ids) {
  didehpc_rrq_controller(config, id)$worker_stop(worker_ids)
}


rrq_check_package_version <- function(local, remote) {
  min_required <- "0.6.21"
  if (remote < min_required) {
    err <- sprintf(
      "Your remote version of rrq (%s) is too old; must be at least %s",
      as.character(remote), min_required)
    fix <- c("To fix, try running one of",
             '  * obj$install_packages("rrq")',
             '  * obj$install_packages("mrc-ide/rrq")')
    stop(paste(c(err, "", fix), collapse = "\n"), call. = FALSE)
  }
  if (local != remote) {
    warning(sprintf(
      "rrq versions differ between local (%s) and remote (%s)",
      as.character(local), as.character(remote)),
      immediate. = TRUE)
  }
}
