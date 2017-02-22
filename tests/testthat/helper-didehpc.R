DIDEHPC_PATH <- "~/net/home/cluster_testing"
PROGRESS <- TRUE
options("didehpc.cluster" = "fi--didemrchnb",
        ## this suppresses all submission progress; better would be to
        ## scope the exints within the test blocks?  It's necessary,
        ## otherwise "delete to end of line" bit in the progress bar
        ## will delete all the test output.
        "didehpc.suppress_progress" = TRUE)

prepare_didehpc_root <- function() {
  file.path(DIDEHPC_PATH, gsub("-", "", as.character(Sys.Date())))
}

prepare_didehpc <- function(name, ..., files = c(...)) {
  root <- prepare_didehpc_root()
  path <- tempfile(paste0(name, "_"), root)
  dir.create(path, FALSE, TRUE)
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
