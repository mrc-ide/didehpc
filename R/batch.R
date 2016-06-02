write_batch <- function(id, root, template, task=TRUE) {
  dat <- if (task) list(task_id=id) else list(worker_id=id)
  filename <- path_batch(root, id)
  dir.create(dirname(filename), FALSE, TRUE)
  writeLines(whisker::whisker.render(template, dat), filename)
  filename
}

read_templates <- function() {
  path <- system.file(package="didewin")
  re <- "^template_(.*)\\.bat$"
  files <- dir(path, re)
  ret <- setNames(vcapply(file.path(path, files), read_lines),
                  sub(re, "\\1", files))
  v <- setdiff(names(ret), "shared")
  setNames(paste(ret[["shared"]], ret[v], sep="\n"), v)
}

batch_templates <- function(context, config, workdir) {
  root <- context::context_root(context)

  ## Build the absolute path to the context on the remote, even if it
  ## differs in drive from the workdir (which really probably is not a
  ## clever idea).
  root <- normalizePath(root)
  context_root <- prepare_path(root, config$shares)
  context_root_abs <- windows_path(file.path(context_root$drive_remote,
                                             context_root$rel))
  context_id <- context$id

  wd <- prepare_path(workdir, config$shares)

  ## In theory we could shorten context_root here if it lies within
  ## the workdir.
  ##
  ## TODO: Date might be wrong, because this is cached.
  r_version <- paste0(R_BITS, "_",
                      paste(unclass(R_VERSION)[[1]], collapse="_"))
  redis_host <- switch(config$cluster,
                       "fi--didemrchnb"="11.0.0.1",
                       "fi--dideclusthn"="12.0.0.1",
                       "")

  dat <- list(hostname=hostname(),
              date=as.character(Sys.Date()),
              didewin_version=as.character(packageVersion("didewin")),
              context_version=as.character(packageVersion("context")),
              r_version=r_version,
              context_workdrive=wd$drive_remote,
              context_workdir=windows_path(wd$rel),
              context_root=context_root_abs,
              context_id=context_id,
              parallel=config$resource$parallel,
              ## NOTE: don't forget the unname()
              network_shares=unname(lapply(config$shares, function(x)
                list(drive=x$drive_remote,
                     path=windows_path(x$path_remote)))),
              rtools=config$rtools,
              redis_host=redis_host)

  lapply(read_templates(), function(x)
    drop_blank(whisker::whisker.render(x, dat)))
}
