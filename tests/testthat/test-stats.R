context("stats")

## There was a bug where specifying a base package caused provisioning
## to fail.
test_that("stats", {
  skip_on_travis()

  owd <- prepare_didehpc("stats")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "stats")
  obj <- didehpc::queue_didehpc(ctx)
  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  expect_is(res, "sessionInfo")
  expect_true("stats" %in% res$basePkgs)
})
