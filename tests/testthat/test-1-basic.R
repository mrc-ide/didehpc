context("basic usage")

test_that("basic", {
  skip_on_travis()

  owd <- prepare_didehpc("basic", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources.R")
  obj <- didehpc::queue_didehpc(ctx)

  expect_is(obj, "queue_didehpc")
  path_lib <- file.path(path, "lib", "windows", obj$config$r_version[1, 1:2])
  expect_true(file.exists(path_lib))
  expect_true(file.exists(file.path(path_lib, "context")))

  ign <- capture.output(dat <- obj$cluster_load())
  expect_is(dat, "dide_clusterload")

  ign <- capture.output(dat <- obj$cluster_load(TRUE))
  expect_is(dat, "dide_clusterload")

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)

  expect_equal(t$status(), "COMPLETE")
  expect_match(obj$dide_id(t), "^[0-9]+$")

  expect_is(res, "sessionInfo")
  expect_true(grepl("Windows Server", res$running, fixed = TRUE))

  ## Logging has worked:
  log <- t$log()
  expect_is(log, "context_log")
  expect_true("hostname" %in% log$title)

  ## Then try something with the source files:
  t2 <- obj$enqueue(make_tree(10))
  phy <- t2$wait(10, progress = FALSE)
  expect_is(phy, "phylo")

  ## And a group:
  grp <- obj$lapply(3:8, quote(make_tree), progress = PROGRESS)
  trees <- grp$wait(20, progress = PROGRESS)
  expect_true(all(vapply(trees, inherits, logical(1), "phylo")))
})
