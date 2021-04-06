context("reconcile")

test_that("reporting on reconciliation shows correct details", {
  expect_message(reconcile_report(NULL),
                 "All job statuses look accurate")

  ids <- c("aaa", "bbb")
  expect_message(
    reconcile_report(data_frame(id = ids, hpc = "ERROR", old = "PENDING")),
    "Tasks have failed while context booting:\n  - aaa\n  - bbb")
  expect_message(
    reconcile_report(data_frame(id = ids, hpc = "ERROR", old = "RUNNING")),
    "Tasks have crashed after starting:\n  - aaa\n  - bbb")
  expect_message(
    reconcile_report(data_frame(id = ids, hpc = "MISSING", old = "RUNNING")),
    "Tasks have gone missing on the cluster:\n  - aaa\n  - bbb")
  expect_message(
    reconcile_report(data_frame(id = ids, hpc = "COMPLETE", old = "RUNNING")),
    "Tasks have started on cluster, unexpectedly stopped:\n  - aaa\n  - bbb")
  expect_message(
    reconcile_report(data_frame(id = ids, hpc = "CANCELLED", old = "PENDING")),
    "Tasks cancelled before starting:\n  - aaa\n  - bbb")
  expect_message(
    reconcile_report(data_frame(id = ids, hpc = "CANCELLED", old = "RUNNING")),
    "Tasks cancelled after starting:\n  - aaa\n  - bbb")
})


test_that("Apply changes to database", {
  ctx <- context::context_save(tempfile())
  id1 <- context::task_save(quote(f(1)), ctx)
  id2 <- context::task_save(quote(f(2)), ctx)
  d <- data_frame(
    id = c(id1, id2),
    new = c("ERROR", "CANCELLED"))
  res <- evaluate_promise(reconcile_update(d, ctx$db))

  expect_null(res$result)
  expect_equal(
    res$messages[[1]],
    sprintf("manually erroring task %s\n", id1))
  expect_equal(
    res$messages[[2]],
    sprintf("manually cancelling task %s\n", id2))

  expect_equal(context::task_status(id1, ctx), "ERROR")
  expect_equal(context::task_status(id2, ctx), "CANCELLED")

  expect_null(reconcile_update(NULL))
})
