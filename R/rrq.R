##' @export
get_rrq_controller.queue_didewin <- function(x, ...) {
  con <- redux::hiredis(host=x$config$cluster)
  rrq::rrq_controller(x$context, con, x$context_envir)
}
