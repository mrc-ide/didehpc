DIDEHPC_PATH <- "~/net/home/cluster_testing"
PROGRESS <- TRUE
options("didehpc.cluster" = "fi--didemrchnb",
        ## this suppresses all submission progress; better would be to
        ## scope the exints within the test blocks?  It's necessary,
        ## otherwise "delete to end of line" bit in the progress bar
        ## will delete all the test output.
        "queuer.progress_suppress" = TRUE)

prepare_didehpc_root <- function() {
  file.path(DIDEHPC_PATH, gsub("-", "", as.character(Sys.Date())))
}

prepare_didehpc_dir <- function(name) {
  root <- prepare_didehpc_root()
  path <- tempfile(paste0(name, "_"), root)
  dir.create(path, FALSE, TRUE)
  path
}

prepare_didehpc <- function(name, ..., files = c(...)) {
  path <- prepare_didehpc_dir(name)
  file.copy(files, path, recursive = TRUE)
  context::context_log_start()
  owd <- setwd(path)
}

wait_pending <- function(t, timeout = 20) {
  times_up <- queuer:::time_checker(timeout)
  while (!times_up()) {
    if (all(t$status() != "PENDING")) {
      return()
    }
    message(".")
    Sys.sleep(0.5)
  }
  stop("Did not start in time")
}

## This comes from provisionr's helper functions;
alter_package_version <- function(path, increase) {
  desc <- file.path(path, "DESCRIPTION")
  d <- read.dcf(desc)
  v <- alter_version(d[, "Version"], increase)
  d[, "Version"] <- v
  write.dcf(d, desc)
  invisible(numeric_version(v))
}

alter_version <- function(v, increase) {
  if (inherits(v, "numeric_version")) {
    as_version <- TRUE
  } else {
    v <- numeric_version(v)
    as_version <- FALSE
  }
  if (increase) {
    i <- length(unclass(v)[[1L]])
    v[[1L, i]] <- v[[1L, i]] + 1L
  } else {
    for (i in rev(seq_along(unclass(v)[[1L]]))) {
      if (v[[1L, i]] > 0L) {
        v[[1L, i]] <- v[[1L, i]] - 1L
        break
      }
    }
  }
  if (as_version) v else as.character(v)
}

read_version <- function(path) {
  numeric_version(read.dcf(file.path(path, "DESCRIPTION"), "Version")[[1]])
}
