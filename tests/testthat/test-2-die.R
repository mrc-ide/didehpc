context("die")

test_that("die", {
  skip_on_travis()

  owd <- prepare_didehpc("basic", "mysources.R")
  on.exit(setwd(owd))

  context::context_log_start()
  path <- "context"
  ## NOTE: This one cannot use the common library because I need to
  ## delete the context package from the library during the test.
  ctx <- context::context_save(path, sources = "mysources.R")
  obj <- didehpc::queue_didehpc(ctx)

  ## Die after startup:
  t1 <- obj$enqueue(q("no", status = 1, runLast = FALSE))
  wait_pending(t1)

  expect_equal(t1$status(), "RUNNING")
  log1 <- obj$dide_log(t1$id)
  expect_match(log1, "Error running task")

  expect_message(ans1 <- obj$task_status_dide(),
                 "Tasks have crashed after starting")
  expect_equal(ans1$id, t1$id)
  expect_equal(ans1$old, "RUNNING")
  expect_equal(ans1$new, "ERROR")

  with_mock(`didehpc:::didehpc_jobstatus` = function(...) stop("lava"),
            expect_message(obj$task_status_dide(),
                           "All job statuses look accurate"))

  ## Weirdly finish after startup (should not happen)
  t2 <- obj$enqueue(q("no", status = 0, runLast = FALSE))
  wait_pending(t2)
  Sys.sleep(1)
  expect_equal(t2$status(), "RUNNING")
  log2 <- obj$dide_log(t2$id)
  expect_match(log2, "Quitting")

  expect_message(ans2 <- obj$task_status_dide(),
                 "unexpectedly stopped")
  expect_equal(ans2$id, t2$id)
  expect_equal(ans2$old, "RUNNING")
  expect_equal(ans2$hpc, "COMPLETE")
  expect_equal(ans2$new, "ERROR")

  with_mock(`didehpc:::didehpc_jobstatus` = function(...) stop("lava"),
            expect_message(obj$task_status_dide(),
                           "All job statuses look accurate"))

  ## Cancel the job
  t3 <- obj$enqueue(Sys.sleep(3600))
  wait_pending(t3)
  expect_equal(didehpc_cancel(obj$config, obj$dide_id(t3)), "OK")
  expect_equal(t3$status(), "RUNNING")
  log3 <- obj$dide_log(t3$id)
  expect_match(log3, "Terminate batch job")
  expect_match(log3, "Error running task")

  expect_message(ans3 <- obj$task_status_dide(),
                 "Tasks cancelled")
  expect_equal(ans3$id, t3$id)
  expect_equal(ans3$old, "RUNNING")
  expect_equal(ans3$new, "CANCELLED")

  with_mock(`didehpc:::didehpc_jobstatus` = function(...) stop("lava"),
            expect_message(obj$task_status_dide(),
                           "All job statuses look accurate"))

  ## Let's do it all again, manually:
  obj$db$mset(c(t1$id, t2$id, t3$id), rep("RUNNING", 3L), "task_status")
  tmp <- task_status_dide_compare(obj, NULL)
  ids <- c(t1$id, t2$id, t3$id)
  expect_true(all(ids %in% tmp$id))
  tmp <- tmp[match(ids, tmp$id), ]
  expect_equal(tmp$old, rep("RUNNING", 3L))
  expect_equal(tmp$new, rep(c("ERROR", "CANCELLED"), 2:1))

  expect_message(task_status_dide_report(tmp),
                 "Tasks have crashed after starting")
  expect_message(task_status_dide_report(tmp),
                 "Tasks cancelled after starting")

  expect_equal(obj$task_status(ids, FALSE), rep("RUNNING", 3))
  expect_message(task_status_dide_update(obj, tmp),
                 "manually erroring tasks")
  expect_message(task_status_dide_update(obj, tmp),
                 "manually cancelling task")

  expect_is(t1$result(), "simpleError")
  expect_equal(t1$result()$message, "Queued job failure")
  expect_is(t2$result(), "simpleError")
  expect_equal(t2$result()$message, "Queued job failure")
  expect_error(t3$result(), "unfetchable: CANCELLED")

  with_mock(`didehpc:::didehpc_jobstatus` = function(...) stop("lava"),
            expect_message(obj$task_status_dide(),
                           "All job statuses look accurate"))

  ## Another, harder, failure mode is the startup failure.  This one
  ## is a real pain to trigger.  This one should fail OK during
  ## startup using just context code now.
  file.remove("mysources.R")
  t4 <- obj$enqueue(sessionInfo())
  wait_pending(t4)
  expect_equal(t4$status(), "ERROR")
  with_mock(`didehpc:::didehpc_jobstatus` = function(...) stop("lava"),
            expect_message(obj$task_status_dide(),
                           "All job statuses look accurate"))

  ## Then we can get more extreme and delete context or one of its
  ## dependencies.
  path_lib <- context:::path_library(path, "windows", obj$config$r_version)
  unlink(file.path(path_lib, "context"), recursive = TRUE)
  t5 <- obj$enqueue(sessionInfo())
  Sys.sleep(1)
  testthat::try_again(5, {
    Sys.sleep(2)
    log5 <- obj$dide_log(t5)
    expect_match(log5, "Error running task")
  })
  expect_equal(t5$status(), "PENDING")

  expect_message(ans5 <- obj$task_status_dide(),
                 "Tasks have failed while context booting")
  expect_equal(ans5$id, t5$id)
  expect_equal(ans5$old, "PENDING")
  expect_equal(ans5$new, "ERROR")
  with_mock(`didehpc:::didehpc_jobstatus` = function(...) stop("lava"),
            expect_message(obj$task_status_dide(),
                           "All job statuses look accurate"))
})
