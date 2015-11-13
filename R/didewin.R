submit <- function(root, task_ids, config, workdir) {
  ## This is a bit roundabout but it means that multiple tasks
  ## can be submitted fairly atomically (e.g., if some tasks
  ## don't exist).
  f <- function(i, x) {
    batch <- write_batch(x[[i]], config, workdir)
    remote_path(prepare_path(batch, config$shares))
  }
  handles <- context::task_handle(root, task_ids)
  path <- vapply(seq_along(task_ids), f, character(1), handles,
                 USE.NAMES=FALSE)
  ## I don't think that this will work, because the web form
  ## will assign everything to a single ID.  And I doubt that
  ## it will return >1 ID if we ask nicely.
  dide_id <- web_submit(path, config, paste(task_ids, collapse="\n"))
  f_task_id <- path_dide_task_id(root, task_ids)
  f_cluster <- path_dide_cluster(root, task_ids)
  for (i in seq_along(task_ids)) {
    writeLines(dide_id[[i]], f_task_id[[i]])
    writeLines(config$cluster, f_cluster[[i]])
  }
}

build_batch <- function(task_handle, config, workdir) {
  wd <- prepare_path(workdir, config$shares)

  ## Build the absolute path to the context on the remote, even if it
  ## differs in drive from the workdir (which really probably is not a
  ## clever idea).
  context_root <- prepare_path(task_handle$root, config$shares)
  context_root <- windows_path(file.path(context_root$drive_remote,
                                         context_root$rel))
  context_logfile <- windows_path(path_logs(context_root, task_handle$id))

  ## Consider dumping out:
  ##   WMIC NETUSE LIST FULL /FORMAT:CSV
  ## So we can see how the network mappings look
  dat <- list(date=as.character(Sys.time()),
              didewin_version=as.character(packageVersion("didewin")),
              context_version=as.character(packageVersion("context")),
              r_version="64_3_2_2",
              context_task_id=task_handle$id,
              context_workdrive=wd$drive_remote,
              context_workdir=windows_path(wd$rel),
              context_root=context_root,
              context_logfile=context_logfile,
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
write_batch <- function(task_handle, config, workdir) {
  str <- build_batch(task_handle, config, workdir)
  filename <- path_batch(task_handle$root, task_handle$id)
  dir.create(dirname(filename), FALSE, TRUE)
  writeLines(str, filename)
  filename
}
