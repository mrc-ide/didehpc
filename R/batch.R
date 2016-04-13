build_batch <- function(root, task_id, config, workdir) {
  wd <- prepare_path(workdir, config$shares)

  ## Build the absolute path to the context on the remote, even if it
  ## differs in drive from the workdir (which really probably is not a
  ## clever idea).
  context_root <- prepare_path(root, config$shares)
  context_root <- windows_path(file.path(context_root$drive_remote,
                                         context_root$rel))
  context_logfile <- windows_path(path_logs(context_root, task_id))

  ## Consider dumping out:
  ##   WMIC NETUSE LIST FULL /FORMAT:CSV
  ## So we can see how the network mappings look
  r_version <- paste0(R_BITS, "_",
                      paste(unclass(R_VERSION)[[1]], collapse="_"))
  dat <- list(date=as.character(Sys.time()),
              didewin_version=as.character(packageVersion("didewin")),
              context_version=as.character(packageVersion("context")),
              r_version=r_version,
              context_task_id=task_id,
              context_workdrive=wd$drive_remote,
              context_workdir=windows_path(wd$rel),
              context_root=context_root,
              context_logfile=context_logfile,
              parallel=config$resource$parallel,
              ## NOTE: don't forget the unname()
              network_shares=unname(lapply(config$shares, function(x)
                list(drive=x$drive_remote,
                     path=windows_path(x$path_remote)))))

  template <- readLines(system.file("template.bat", package="didewin"))
  drop_blank(whisker::whisker.render(template, dat))
}

## TODO: vectorise this over id s that will be pretty standard.
## However, do do this, come up with the idea of a "task set" that
## shares everything but ID and has a vectorised ID.
write_batch <- function(root, task_id, config, workdir) {
  str <- build_batch(root, task_id, config, workdir)
  filename <- path_batch(root, task_id)
  dir.create(dirname(filename), FALSE, TRUE)
  writeLines(str, filename)
  filename
}
