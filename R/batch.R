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

  network_shares_data <- list(
    drive = lapply(config$shares, "[[", "drive_remote"),
    path = lapply(config$shares, "[[", "path_remote"))
  temp_drive <- remote_drive_temp(config$shares)
  if (is.null(temp_drive)) {
    temp_drive <- available_drive(config$shares, "", "T")
    network_shares_data$drive <- c(network_shares_data$drive, temp_drive)
    network_shares_data$path <- c(network_shares_data$path,
                                  "\\\\fi--didef3.dide.ic.ac.uk\\tmp")
  }
  network_shares_create <- glue_whisker(
    "ECHO mapping {{drive}} -^> {{path}}\nnet use {{drive}} {{path}} /y",
    network_shares_data)
  network_shares_delete <- glue_whisker(
    "ECHO Removing mapping {{drive}}\nnet use {{drive}} /delete /y",
    network_shares_data)

  if (config$resource$parallel) {
    parallel <- paste("ECHO This is a parallel job: will use %CPP_NUMCPUS%",
                      "set CONTEXT_CORES=%CCP_NUMCPUS%",
                      sep = "\n")
  } else {
    parallel <- NULL
  }

  if (config$conan_bootstrap) {
    conan_path_bootstrap <-
      windows_path(path_conan_bootstrap(temp_drive, config$r_version))
  } else {
    conan_path_bootstrap <- NULL
  }

  list(hostname = hostname(),
       date = as.character(Sys.Date()),
       didehpc_version = as.character(packageVersion("didehpc")),
       context_version = as.character(packageVersion("context")),
       conan_version = as.character(packageVersion("conan")),
       r_version = r_version_str,
       network_shares_create = paste(network_shares_create, collapse = "\n"),
       network_shares_delete = paste(network_shares_delete, collapse = "\n"),
       context_workdrive = workdir$drive_remote,
       context_workdir = windows_path(workdir$rel),
       context_root = context_root_abs,
       context_id = context_id,
       conan_path_bootstrap = conan_path_bootstrap,
       r_libs_user = r_libs_user,
       parallel = parallel,
       redis_host = redis_host(config$cluster),
       worker_timeout = config$worker_timeout,
       rrq_worker_log_path = path_worker_logs(NULL),
       log_path = path_logs(NULL),
       cluster_name = config$cluster,
       use_java = config$use_java,
       java_home = config$java_home)
}

batch_templates <- function(context_root, context_id, config, workdir) {
  templates <- read_templates()

  dat <- template_data(context_root, context_id, config, workdir)

  if (is.null(config$worker_resource)) {
    dat_worker <- dat
  } else {
    ## Apply overrides for runners, if present (they won't be most of
    ## the time).
    config$resource <- config$worker_resource
    dat_worker <- template_data(context_root, context_id, config, workdir)
  }

  render <- function(x, dat) {
    drop_blank(glue_whisker(x, dat))
  }

  list(conan = render(templates$conan, dat),
       runner = render(templates$runner, dat),
       rrq_worker = render(templates$rrq_worker, dat_worker))
}


## The batch files make reference to many paths, which need to be
## consistent. We'll try and collect them here.
batch_data <- function(context_root, context_id, config) {
  ## TODO: We need to get to the bottom of what workdir was originally
  ## for!
  workdir <- config$workdir %||% getwd()
  templates <- batch_templates(context_root, context_id, config, workdir)
  context_root_remote <- remote_path(context_root, config$shares)

  paths_tail <- list(
    root = NULL,
    conan = "conan",
    batch = "batch",
    lib = path_library(NULL, config$r_version),
    ## TODO: log => logs
    log = path_logs(NULL),
    worker_log = path_worker_logs(NULL))

  paths <- list(
    local = lapply(paths_tail, function(x)
      file_path(context_root, x)),
    remote = lapply(paths_tail, function(x)
      windows_path(file_path(context_root_remote, x))))
  paths$local$workdir <- workdir
  paths$remote$workdir <- remote_path(workdir, config$shares)

  list(templates = templates, paths = paths)
}
