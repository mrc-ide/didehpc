context("path with spaces")

test_that("spaces", {
  skip_on_travis()

  owd <- prepare_didehpc("path with spaces", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources.R")
  obj <- didehpc::queue_didehpc(ctx)

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  expect_equal(names(res$otherPkgs), "ape")
})
