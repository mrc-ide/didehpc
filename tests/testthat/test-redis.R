context("redis")

## This tests that the defaults have been appropriately configured.
## We should be able to hit the redis server with a minimum of hassle;
## passing host/port through should not be required because it's set
## up already.
test_that("redis: ping", {
  skip_on_travis()
  skip_if_not_installed("redux")

  owd <- prepare_didewin("redis")
  on.exit(setwd(owd))

  src <- provisionr::package_sources(repos = "drat://richfitz")
  ctx <- context::context_save(path = "context",
                               packages = "redux",
                               package_sources = src)
  obj <- didewin::queue_didewin(ctx)

  t <- obj$enqueue(redux::hiredis()$PING())
  res <- t$wait(10, progress = FALSE)
  expect_is(res, "redis_status")
  expect_equal(unclass(res), "PONG")
})
