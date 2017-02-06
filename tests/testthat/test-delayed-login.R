context("delayed login")

test_that("delayed", {
  skip_on_travis()

  owd <- prepare_didewin("delayed", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources.R")
  obj <- didewin::queue_didewin(ctx, initialise = FALSE)

  path_lib <- file.path(path, "lib")
  expect_false(file.exists(path_lib))
  expect_false(obj$logged_in)

  t <- obj$enqueue(sessionInfo())
  expect_true(obj$logged_in)
  expect_true(file.exists(path_lib))
  res <- t$wait(10, progress = FALSE)

  expect_equal(names(res$otherPkgs), "ape")
})
