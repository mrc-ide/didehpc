context("workers usage")

test_that("workers", {
  skip_on_travis()

  sources <- "mysources.R"
  owd <- prepare_didehpc("workers", sources)
  on.exit(setwd(owd))

  config <- didehpc::didehpc_config(use_workers = TRUE,
                                    worker_timeout = 3600)

  path <- "context"
  ctx <- context::context_save(path = path, sources = sources)
  obj <- didehpc::queue_didehpc(ctx, config = config)

  expect_equal(obj$task_list(), character(0))

  r <- obj$rrq_controller()
  expect_is(r, "rrq_controller")
  expect_equal(r$queue_length(), 0L)

  t <- obj$enqueue(sessionInfo())
  expect_equal(r$queue_length(), 1L)

  wid <- obj$submit_workers(5, progress = FALSE)
  dat <- t$wait(10)
  expect_is(dat, "sessionInfo")
  r$worker_log_tail(n = Inf)

  ## Then we start the fun bit:
  x <- runif(30)
  res <- obj$lapply(x, quote(slow_double), timeout = 30, progress = FALSE)
  expect_equal(res, as.list(x * 2))

  ans <- r$send_message_and_wait("STOP")
  expect_equal(unname(ans), rep(list("BYE"), length(wid)))
})
