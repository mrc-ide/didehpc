context("redis")

## This tests that the defaults have been appropriately configured.
## We should be able to hit the redis server with a minimum of hassle;
## passing host/port through should not be required because it's set
## up already.
test_that("redis: ping", {
  skip_on_travis()
  skip_if_not_installed("redux")

  owd <- prepare_didehpc("redis")
  on.exit(setwd(owd))

  ctx <- context::context_save(path = "context",
                               packages = "redux")
  obj <- didehpc::queue_didehpc(ctx)

  t <- obj$enqueue(redux::hiredis()$PING())
  res <- t$wait(10, progress = FALSE)
  expect_is(res, "redis_status")
  expect_equal(unclass(res), "PONG")
})
