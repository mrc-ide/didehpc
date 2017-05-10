context("sql")

test_that("sql", {
  skip_on_travis()
  skip_if_not_installed("RPostgres")
  skip_if_not_installed("RPostgreSQL")

  owd <- prepare_didehpc("sql", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources.R",
                               storage_type = storage_driver_psql())

  obj <- didehpc::queue_didehpc(ctx, config = list(use_common_lib = COMMON))

  expect_is(obj, "queue_didehpc")

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)

  grp <- obj$lapply(1:4, quote(sin))
  ans <- grp$wait(10)
  expect_equal(ans, as.list(sin(1:4)))

  ## Cleanup
  obj$db$destroy()
})
