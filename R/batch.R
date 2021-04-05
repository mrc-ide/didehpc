write_batch <- function(id, root, template, dat) {
  filename <- path_batch(root, id)
  dir.create(dirname(filename), FALSE, TRUE)
  if (!file.exists(filename)) {
    ## This is for debugging - allowing me to edit the batch files and
    ## relaunch without overwriting.
    writeLines(whisker::whisker.render(template, dat), filename)
  }
  filename
}

read_templates <- function() {
  path <- system.file(package = "didehpc")
  re <- "^template_(.*)\\.bat$"
  files <- dir(path, re)
  dat <- setNames(lapply(file.path(path, files), read_lines),
                  sub(re, "\\1", files))
  list(
    conan = dat$conan,
    runner = paste(dat$shared, dat$runner, sep = "\n"),
    rrq_worker = paste(dat$shared, dat$rrq_worker, sep = "\n"))
}

template_data <- function(context_root, context_id, config, workdir) {
  ## Work out both of our paths on the remote machine; the context
  ## root and the workdir
  context_root <- prepare_path(context_root, config$shares)
  workdir <- prepare_path(workdir, config$shares)

  ## Same path, absolute, that will be used remotely
  context_root_abs <- windows_path(
    file.path(context_root$drive_remote, context_root$rel))

  r_version_str <- paste(unclass(config$r_version)[[1]], collapse = "_")
  r_libs_user <- windows_path(path_library(context_root_abs, config$r_version))

  ## NOTE: don't forget the unname()
  network_shares <- unname(lapply(config$shares, function(x)
    list(drive = x$drive_remote, path = windows_path(x$path_remote))))

  temp_drive <- remote_drive_temp(config$shares)
  if (is.null(temp_drive)) {
    temp_drive <- available_drive(config$shares, "", "T")
    network_shares <- c(
      network_shares,
      list(list(drive = temp_drive,
                path = "\\\\fi--didef3.dide.ic.ac.uk\\tmp")))
  }

  ## Unconditionally for now
  conan_root_bootstrap <-
    windows_path(path_conan_bootstrap(temp_drive, config$r_version))

  rtools <- rtools_versions(temp_drive, config$r_version)

  list(hostname = hostname(),
       date = as.character(Sys.Date()),
       didehpc_version = as.character(packageVersion("didehpc")),
       context_version = as.character(packageVersion("context")),
       conan_version = as.character(packageVersion("conan")),
       r_version = r_version_str,
       network_shares = network_shares,
       context_workdrive = workdir$drive_remote,
       context_workdir = windows_path(workdir$rel),
       context_root = context_root_abs,
       context_id = context_id,
       conan_root_bootstrap = conan_root_bootstrap,
       r_libs_user = r_libs_user,
       rtools = rtools,
       parallel = config$resource$parallel,
       redis_host = redis_host(config$cluster),
       rrq_key_alive = config$rrq_key_alive,
       worker_timeout = config$worker_timeout,
       rrq_worker_log_path = path_worker_logs(NULL),
       log_path = path_logs(NULL),
       cluster_name = config$cluster,
       use_java = config$use_java,
       java_home = config$java_home)
}

batch_templates <- function(context_root, context_id, config, workdir) {
  dat <- template_data(context_root, context_id, config, workdir)
  templates <- read_templates()
  lapply(templates, function(x)
    drop_blank(whisker::whisker.render(x, dat)))
}
