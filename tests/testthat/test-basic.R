context("basic usage")

test_that("basic", {
  skip_on_travis()

  owd <- prepare_didewin("basic", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources.R")
  obj <- didewin::queue_didewin(ctx)

  expect_is(obj, "queue_didewin")
  path_lib <- file.path(path, "lib", "windows", obj$config$r_version[1, 1:2])
  expect_true(file.exists(path_lib))
  expect_true(file.exists(file.path(path_lib, "context")))

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)

  expect_equal(t$status(), "COMPLETE")

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
  grp <- obj$lapply(3:8, quote(make_tree), progress = FALSE)
  trees <- grp$wait(20, progress = FALSE)
  expect_true(all(vapply(trees, inherits, logical(1), "phylo")))
})
