context("stats")

## There was a bug where specifying a base package caused provisioning
## to fail.
test_that("stats", {
  skip_on_travis()

  owd <- prepare_didewin("stats")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "stats")
  obj <- didewin::queue_didewin(ctx)
  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  expect_is(res, "sessionInfo")
  expect_true("stats" %in% res$basePkgs)
})
