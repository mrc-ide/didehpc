## TODO: it might be worth checking to see how slow this is just with
## whisker and preparing a single task, returning something that just
## needs the task id?
build_batch <- function(root, id, config, workdir, worker_id=NULL) {
  wd <- prepare_path(workdir, config$shares)

  ## Build the absolute path to the context on the remote, even if it
  ## differs in drive from the workdir (which really probably is not a
  ## clever idea).
  root <- normalizePath(root)

  context_root <- prepare_path(root, config$shares)
  context_root_abs <- windows_path(file.path(context_root$drive_remote,
                                             context_root$rel))

  ## In theory we could shorten context_root here if it lies within
  ## the workdir.

  r_version <- paste0(R_BITS, "_",
                      paste(unclass(R_VERSION)[[1]], collapse="_"))
  dat <- list(hostname=hostname(),
              date=as.character(Sys.time()),
              didewin_version=as.character(packageVersion("didewin")),
              context_version=as.character(packageVersion("context")),
              r_version=r_version,
              context_workdrive=wd$drive_remote,
              context_workdir=windows_path(wd$rel),
              context_root=context_root_abs,
              parallel=config$resource$parallel,
              ## NOTE: don't forget the unname()
              network_shares=unname(lapply(config$shares, function(x)
                list(drive=x$drive_remote,
                     path=windows_path(x$path_remote)))),
              rtools=config$rtools)

  if (is.null(worker_id)) {
    dat$context_task_id <- id
  } else {
    dat$context_id <- id
    dat$worker_id <- worker_id
  }

  template <- readLines(system.file("template.bat", package="didewin"))
  drop_blank(whisker::whisker.render(template, dat))
}

## TODO: vectorise this over id s that will be pretty standard.
## However, do do this, come up with the idea of a "task set" that
## shares everything but ID and has a vectorised ID.
write_batch <- function(root, task_id, config, workdir, worker_id=NULL) {
  str <- build_batch(root, task_id, config, workdir, worker_id)
  filename <- path_batch(root, task_id)
  dir.create(dirname(filename), FALSE, TRUE)
  writeLines(str, filename)
  filename
}
