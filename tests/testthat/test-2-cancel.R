context("cancel")

test_that("cancel", {
  skip_on_travis()

  owd <- prepare_didehpc("basic", "mysources.R")
  on.exit(setwd(owd))

  context::context_log_start()
  path <- "context"
  ctx <- context::context_save(path, packages = "stats")
  obj <- didehpc::queue_didehpc(ctx)

  ## Via unsubmit directly
  t1 <- obj$enqueue(Sys.sleep(3600))
  wait_pending(t1)

  expect_equal(obj$unsubmit(t1$id), "OK")
  expect_equal(obj$unsubmit(t1$id), "NOT_RUNNING")
  expect_equal(t1$status(), "CANCELLED")
  expect_is(t1$log(), "context_log")

  ## Via deletion
  t2 <- obj$enqueue(Sys.sleep(3600))
  wait_pending(t2)

  expect_true(obj$task_delete(t2$id))
  expect_false(obj$task_delete(t2$id))
  expect_equal(obj$unsubmit(t2$id), "NOT_RUNNING")
  expect_equal(t2$status(), "MISSING")

  ## Jobs that complete
  t3 <- obj$enqueue(sin(1))
  res <- t3$wait(20, progress = FALSE)
  expect_equal(obj$unsubmit(t3$id), "NOT_RUNNING")
  expect_equal(t3$result(), sin(1))

  ## Group of jobs
  grp <- obj$lapply(rep(3600, 4), quote(Sys.sleep))
  wait_pending(grp)
  expect_equal(obj$unsubmit(grp$ids), rep("OK", 4))
  expect_equal(obj$unsubmit(grp$ids), rep("NOT_RUNNING", 4))
  expect_equal(grp$results()[[1]]$message, "Task cancelled")
})
