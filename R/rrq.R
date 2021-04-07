rrq_init <- function(rrq, config) {
  ## All workers read the full context
  rrq$envir(rrq_context_loader(), notify = FALSE)
  ## NOTE: we should consider a heartbeat here
  ##
  ## NOTE: Jobs that use rrq controller *from* a job risk a
  ## deadlock because we could request more work than we have
  ## workers
  rrq$worker_config_save("localhost", timeout = config$worker_timeout,
                         queue = c("default", "context"),
                         overwrite = FALSE)
}


rrq_context_loader <- function() {
  create_env <- new.env(parent = globalenv())
  create <- function(envir) {
    context_root <- Sys.getenv("CONTEXT_ROOT")
    context_id <- Sys.getenv("CONTEXT_ID")
    if (context_root == "" | context_id == "") {
      stop(sprintf(
        "Environment variables incorrect: CONTEXT_ROOT: '%s' CONTEXT_ID: '%s')",
        CONTEXT_ROOT, CONTEXT_ID))
    }
    ctx <- context::context_read(context_id, context_root)
    context::context_load(ctx, envir)
  }
  environment(create) <- create_env
}


submit_workers <- function(obj, data, n, timeout = 600, progress = NULL) {
  root <- obj$root$path
  config <- obj$config
  template <- obj$templates$rrq_worker
  client <- obj$client
  batch_template <- data$templates$rrq_worker

  ## Will be shared across all jobs submitted
  job_template <- config$template
  cluster <- config$cluster
  resource_type <- config$resource$type
  resource_count <- config$resource$count

  base <- ids::adjective_animal()
  names <- sprintf("%s_%d", base, seq_len(n))

  rrq <- obj$rrq_controller()
  rrq_key_alive <- rrq::rrq_expect_worker(rrq, names)

  message(sprintf("Submitting %d %s with base name '%s'",
                  n, ngettext(n, "worker", "workers"), base))
  p <- queuer::progress_timeout(n, timeout, label = "submitting ",
                                show = progress)

  dir.create(data$paths$local$batch, FALSE, TRUE)
  dir.create(data$paths$local$worker_log, FALSE, TRUE)
  for (nm in names) {
    dat <- list(rrq_worker_id = nm, rrq_key_alive = rrq_key_alive)
    base <- paste0(nm, ".bat")
    batch <- file.path(data$paths$local$batch, base)
    writeLines(glue_whisker(batch_template, dat), batch)
    path <- windows_path(file.path(data$paths$remote$batch, base))
    p()
    dide_id <- client$submit(path, nm, job_template, cluster,
                             resource_type, resource_count)
  }

  rrq::worker_wait(rrq, rrq_key_alive, timeout = timeout, progress = progress)
}


rrq_redis_host <- function(cluster) {
  paste0(cluster, ".dide.ic.ac.uk")
}


check_rrq_enabled <- function(config) {
  if (!(isTRUE(obj$config$use_rrq) || isTRUE(obj$config$use_workers))) {
    stop("workers not enabled")
  }
}
