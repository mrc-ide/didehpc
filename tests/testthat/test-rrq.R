context("rrq usage")

test_that("rrq", {
  skip_on_travis()

  sources <- c("mysources.R", "rrq-controller.R")
  owd <- prepare_didewin("rrq", sources)
  on.exit(setwd(owd))

  config <- didewin::didewin_config(use_rrq = TRUE, worker_timeout = 3600)

  path <- "context"
  ctx <- context::context_save(path = path, sources = sources)
  expect_error(didewin::queue_didewin(ctx, config = config),
               "context must contain a unique_value")

  ctx <- context::context_save(path = path, sources = sources,
                               unique_value = ids::random_id())
  obj <- didewin::queue_didewin(ctx, config = config)

  r <- obj$rrq_controller()
  expect_equal(r$workers_info(), setNames(list(), character(0)))
  expect_equal(r$workers_len(), 0L)

  wid <- obj$submit_workers(2, progress = PROGRESS)
  expect_is(wid, "character")

  res <- simulation_local(10, 14, r)
  expect_is(res, "matrix")

  t <- obj$enqueue(simulation(10, 14))
  res2 <- t$wait(100, progress = PROGRESS)
  expect_is(res2, "matrix")

  ## Then scale up:
  ## t <- obj$enqueue(simulation(50, 20))
  ## ans <- obj$submit_workers(6, TRUE)
  ## t$wait(100)
})
