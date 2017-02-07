context("linux")

test_that("linux", {
  skip_on_travis()
  skip_if_not_installed("buildr")

  owd <- prepare_didewin("linux", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources.R")
  cfg <- didewin_config(cluster = "fi--didelxhn")
  obj <- didewin::queue_didewin(ctx, config = cfg)

  path_lib <- file.path(path, "lib", "linux", obj$config$r_version[, 1:2])
  expect_true(file.exists(file.path(path_lib, "ape")))

  t1 <- obj$enqueue(sessionInfo())
  res1 <- t1$wait(10, progress = FALSE)
  expect_is(res1, "sessionInfo")
  expect_equal(res1$R.version$os, "linux-gnu")

  t2 <- obj$enqueue(make_tree(10))
  phy <- t2$wait(10, progress = FALSE)
  expect_is(phy, "phylo")

  expect_is(t2$log(), "context_log")
  log <- obj$dide_log(t2)
  expect_true(grepl("Quitting", log))
})
