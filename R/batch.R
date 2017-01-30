## TODO: I'd prefer it if the paths in the generated templates were
## *relative* paths, not absolute paths.  This is an issue for the
## task runner, the context root and the log
write_batch <- function(id, root, template, task = TRUE, linux = FALSE) {
  dat <- if (task) list(task_id = id) else list(worker_id = id)
  filename <- path_batch(root, id, linux)
  dir.create(dirname(filename), FALSE, TRUE)
  if (!file.exists(filename)) {
    ## This is for debugging - allowing me to edit the batch files
    writeLines(whisker::whisker.render(template, dat), filename)
  }
  filename
}

read_templates <- function(ext) {
  path <- system.file(package = "didewin")
  re <- sprintf("^template_(.*)\\.%s$", ext)
  files <- dir(path, re)
  ret <- setNames(vcapply(file.path(path, files), read_lines),
                  sub(re, "\\1", files))
  v <- setdiff(names(ret), "shared")
  setNames(paste(ret[["shared"]], ret[v], sep = "\n"), v)
}

batch_templates <- function(context, config, workdir) {
  linux <- linux_cluster(config$cluster)
  root <- context$root$path

  ## Build the absolute path to the context on the remote, even if it
  ## differs in drive from the workdir (which really probably is not a
  ## clever idea).
  root <- normalizePath(root, mustWork = TRUE)
  context_root <- prepare_path(root, config$shares)
  if (linux) {
    context_root_abs <- unix_path(file.path(context_root$drive_remote,
                                            context_root$rel))
  } else {
    context_root_abs <- windows_path(file.path(context_root$drive_remote,
                                               context_root$rel))
  }
  context_id <- context$id

  wd <- prepare_path(workdir, config$shares)

  ## In theory we could shorten context_root here if it lies within
  ## the workdir.
  ##
  ## TODO: Date might be wrong, because this is cached.
  if (linux) {
    r_version <- as.character(config$r_version)
    context_workdir <- unix_path(file.path(wd$drive_remote, wd$rel))
  } else {
    r_version <- paste0("64_", paste(unclass(config$r_version)[[1]],
                                     collapse = "_"))
    context_workdir <- windows_path(wd$rel)
  }

  dat <- list(hostname = hostname(),
              date = as.character(Sys.Date()),
              didewin_version = as.character(packageVersion("didewin")),
              context_version = as.character(packageVersion("context")),
              r_version = r_version,
              context_workdrive = wd$drive_remote,
              context_workdir = context_workdir,
              context_root = context_root_abs,
              context_id = context_id,
              parallel = config$resource$parallel,
              redis_host = redis_host(config$cluster),
              rrq_key_alive = config$rrq_key_alive,
              worker_timeout = config$worker_timeout,
              worker_log_path = path_worker_logs(NULL),
              log_path = path_logs(NULL))

  if (!linux) {
    ## NOTE: don't forget the unname()
    dat$network_shares <-
      unname(lapply(config$shares, function(x)
        list(drive = x$drive_remote, path = windows_path(x$path_remote))))
    dat$rtools <- if (needs_rtools(config, context)) rtools_info(config) else NULL
  }

  templates <- read_templates(if (linux) "sh" else "bat")
  lapply(templates, function(x)
    drop_blank(whisker::whisker.render(x, dat)))
}
