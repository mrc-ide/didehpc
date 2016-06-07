##' @export
get_rrq_controller.queue_didewin <- function(x, ...) {
  con <- redux::hiredis(host=x$config$cluster)
  rrq::rrq_controller(x$context, con, x$context_envir)
}

initialise_rrq <- function(obj) {
  if (obj$config$use_rrq_workers) {
    root <- context::context_root(obj)
    ## TODO: This is annoying because it means that all workers
    ## will share a key across multiple invocations.  So this may
    ## change in future.  Probably this should happen in the
    ## config bit but that requires fixing that so it knows about
    ## workers.
    obj$config$rrq_key_alive <- rrq::rrq_key_worker_alive(obj$context$id)
    loadNamespace("rrq") # force presence of these packages
    dir.create(path_rrq_worker_logs(root), FALSE, TRUE)
    repos <- c(CRAN="https://cran.rstudio.com",
               richfitz="https://richfitz.github.io/drat/")
    path_lib <- file.path(root, "R", R_PLATFORM, R_VERSION)
    ## TODO: duplicated all over the show:
    r_version_2 <- as.character(R_VERSION[1, 1:2]) # used for talking to CRAN
    context::cross_install_packages(
      path_lib, "windows", r_version_2, repos, "rrq")
    ## TODO: once settled, make optional.
    dest <- file.path(root, "bin", "rrq_worker")
    file.copy(system.file("rrq_worker_bootstrap", package="rrq"), dest)
  }
}
