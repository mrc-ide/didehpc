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

## This needs some sorting out in the package.
##
## One option would be for context to load a "last context" somewhere
## in its environment that we could hook onto.  I think that
## redux::hiredis() will Just Work with REDIS_HOST set, if not, set
## REDIS_URL
##
## Then consider using the config approach here to get the redis host
## information out?  This would all go into rrq of course.
get_rrq_controller <- function(x, ...) {
  con <- redux::hiredis(host = Sys.getenv("REDIS_HOST"))
  ctx <- context::context_read(Sys.getenv("CONTEXT_ID"),
                               Sys.getenv("CONTEXT_ROOT"))
  ctx$envir <- .GlobalEnv
  rrq::rrq_controller(ctx, con)
}

simulation_local <- function(nsteps, nsamples, r) {
  ret <- matrix(NA_real_, nsteps + 1, nsamples)
  x <- ret[1L, ] <- rep(0, nsamples)
  for (i in seq_len(nsteps)) {
    ret[i + 1L,] <- x <-
      sort(unlist(r$lapply(x, slow_rnorm)))
  }
  ret
}
