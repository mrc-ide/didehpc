simulation <- function(nsteps, nsamples) {
  r <- get_rrq_controller()

  ret <- matrix(NA_real_, nsteps + 1, nsamples)
  x <- ret[1L, ] <- rep(0, nsamples)
  for (i in seq_len(nsteps)) {
    ret[i + 1L,] <- x <- sort(unlist(r$lapply(x, slow_rnorm)))
  }
  ret
}

slow_rnorm <- function(x) {
  Sys.sleep(sqrt(abs(x)) / 3)
  rnorm(1, x)
}

get_rrq_controller <- function(x, ...) {
  queue_id <- Sys.getenv("RRQ_QUEUE_ID", "")
  stopifnot(queue_id != "")
  rrq::rrq_controller(queue_id)
}

simulation_local <- function(nsteps, nsamples, r) {
  ret <- matrix(NA_real_, nsteps + 1, nsamples)
  x <- ret[1L, ] <- rep(0, nsamples)
  for (i in seq_len(nsteps)) {
    ret[i + 1L,] <- x <- sort(unlist(r$lapply(x, slow_rnorm)))
  }
  ret
}
