context("stan usage")

test_that("stan", {
  skip_on_travis()
  skip_if_not_installed("rstan")

  ## TODO: this triggers a fairly unpleasant and slow installation of
  ## BH.  It would be nice to be able to skip that by pointing things
  ## at a repo that already has it, I think, or to see if we can get
  ## stan to work without BH.
  owd <- prepare_didewin("stan", "model.stan")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "rstan")
  obj <- didewin::queue_didewin(ctx)

  expect_true(obj$config$rtools)
  expect_true(needs_rtools(obj$config, obj$context))

  N <- 1000
  y <- rbinom(N, 1, 0.7)
  data <- list(N = N, y = y)
  t <- obj$enqueue(stan("model.stan", data = data))
  expect_error(t$wait(10, progress = FALSE), "task not returned in time")
  expect_equal(t$status(), "RUNNING")
  res <- t$wait(100, progress = FALSE)
  expect_is(res, "stanfit")
})
