random_walk <- function(x, n_steps) {
  ret <- numeric(n_steps)
  for (i in seq_len(n_steps)) {
    x <- rnorm(1, x)
    ret[[i]] <- x
  }
  ret
}
