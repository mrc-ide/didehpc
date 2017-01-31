DIDEWIN_PATH <- "~/net/home/cluster_testing"
options("didewin.cluster" = "fi--didemrchnb",
        ## this suppresses all submission progress; better would be to
        ## scope the exints within the test blocks?  It's necessary,
        ## otherwise "delete to end of line" bit in the progress bar
        ## will delete all the test output.
        "didewin.suppress_progress" = TRUE)

prepare_didewin_root <- function() {
  file.path(DIDEWIN_PATH, gsub("-", "", as.character(Sys.Date())))
}

prepare_didewin <- function(name, ..., files = c(...)) {
  root <- prepare_didewin_root()
  path <- tempfile(paste0(name, "_"), root)
  dir.create(path, FALSE, TRUE)
  file.copy(files, path, recursive = TRUE)
  owd <- setwd(path)
}
