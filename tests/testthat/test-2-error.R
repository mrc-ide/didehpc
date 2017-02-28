context("errors")

test_that("error", {
  skip_on_travis()

  owd <- prepare_didehpc("error", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, sources = "mysources.R")
  obj <- didehpc::queue_didehpc(ctx)

  t1 <- obj$enqueue(sessionInfo())
  t2 <- obj$enqueue(pass_through(10))
  t3 <- obj$enqueue(pass_through(-10))

  res1 <- t1$wait(10, progress = FALSE)
  expect_equal(t1$status(), "COMPLETE")

  res2 <- t2$wait(10, progress = FALSE)
  expect_equal(res2, 20)
  log2 <- obj$dide_log(t2)
  expect_match(log2, "Quitting\n*$")

  res3 <- t3$wait(10, progress = FALSE)
  expect_is(res3, "context_task_error")
  log3 <- obj$dide_log(t3)
  expect_match(log3, "Error running task\n*$")
})
