build_batch <- function(task_handle, config, workdir) {
  wd <- prepare_path(workdir, config$shares)

  ## Build the absolute path to the context on the remote, even if it
  ## differs in drive from the workdir (which really probably is not a
  ## clever idea).
  context_root <- prepare_path(task_handle$root, config$shares)
  context_root <- windows_path(file.path(context_root$drive_remote,
                                         context_root$rel))

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
  prepare_path(filename, config$shares)
}

## Fully explicit one-shot submission for testing:
##
## Determine what we do if a file is already submitted?
submit1_ <- function(expr, context, config, workdir=getwd()) {
  task_handle <- context::save_task(expr, context)
  batch <- write_batch(task_handle, config, workdir)
  ## TODO: It would be good to know what the timeout is on the login
  ## so that we could login only when it is likely to be useful.
  ## Because it might prompt for user input it's not really ideal for
  ## use within functions that might be called in scripts.
  web_submit(remote_path(batch), config)
}
