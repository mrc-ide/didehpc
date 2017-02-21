context("sql")

test_that("sql", {
  id <- ids::random_id()
  db <- storage_driver_psql_create(NULL, id, NULL)

  skip_on_travis()
  skip_if_not_installed("RPostgres")

  owd <- prepare_didewin("sql", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources.R",
                               storage_type = storage_driver_psql())

  obj <- didewin::queue_didewin(ctx)

  expect_is(obj, "queue_didewin")
  path_lib <- file.path(path, "lib", "windows", obj$config$r_version[1, 1:2])
  expect_true(file.exists(path_lib))
  expect_true(file.exists(file.path(path_lib, "context")))

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)

  ## This *should* have produced a progress bar
  grp <- obj$lapply(1:100, quote(sin))
})
