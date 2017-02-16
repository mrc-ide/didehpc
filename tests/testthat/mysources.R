## Used in basic.R and elsewhere
make_tree <- function(nspp) {
  message("I am building a tree!")
  ## Sys.sleep(10)
  ape::rtree(nspp)
}

## used in test-error.R
error_if_negative <- function(x) {
  if (x < 0) {
    stop("Need positive x")
  }
  x
}

pass_through <- function(x) {
  error_if_negative(x) * 2
}

slow_double <- function(x) {
  Sys.sleep(runif(1, 0, x))
  x * 2
}

slow_rnorm <- function(x) {
  Sys.sleep(sqrt(abs(x)) / 3)
  rnorm(1, x)
}
