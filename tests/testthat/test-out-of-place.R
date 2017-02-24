context("out-of-place")

test_that("out-of-place", {
  skip_on_travis()
  skip_if_not_installed("syncr")

  context::context_log_start()
  workdir <- prepare_didehpc_dir("oop")
  path <- file.path(workdir, "context")

  ctx <- context::context_save(path = path, sources = "oop-R/myfuns.R")

  obj <- didehpc::queue_didehpc(ctx,
                                config = list(workdir = workdir),
                                sync = c("oop-data/data1",
                                         "oop-data/foo.csv"))

  expect_true(file.exists(file.path(workdir, "oop-R/myfuns.R")))
  expect_true(file.exists(file.path(workdir, "oop-R/myfuns.R")))
  expect_true(file.exists(file.path(workdir, "oop-data/data1")))
  expect_false(file.exists(file.path(workdir, "oop-data/data2")))

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(5, progress = FALSE)

  expect_is(res, "sessionInfo")
  expect_true("context" %in% names(res$loadedOnly))

  t <- obj$enqueue(read())
  res <- t$wait(5, progress = FALSE)
  expect_equal(res, read())

  t <- obj$enqueue(read.csv("oop-data/data1/data1.csv"))
  res <- t$wait(5, progress = FALSE)
  expect_equal(res, read.csv("oop-data/data1/data1.csv"))

  t <- obj$enqueue(read.csv("oop-data/data2/data2.csv"))
  res <- t$wait(5, progress = FALSE)
  expect_is(res, "context_task_error")
})
