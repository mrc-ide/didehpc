context("common_lib")

test_that("common_lib", {
  skip_on_travis()

  owd <- prepare_didewin("common_lib", "mysources.R")
  on.exit(setwd(owd))

  config <- didewin::didewin_config(r_version = "3.2.4", use_common_lib = FALSE)
  expect_null(config$common_lib)
  config <- didewin::didewin_config(r_version = "3.2.4", use_common_lib = TRUE)
  expect_is(config$common_lib, "path_mapping")
  p <- file.path(config$common_lib$path_local, config$common_lib$rel)
  expect_true(file.exists(p))
  expect_true(file.exists(file.path(p, "context")))

  path <- "context"
  ctx <- context::context_save(path = path, sources = "mysources.R")

  obj <- didewin::queue_didewin(ctx, config = config)

  t <- obj$enqueue(.libPaths())
  res <- t$wait(5, progress = FALSE)
  expect_equal(
    res[[2L]],
    unix_path(file.path(config$common_lib$drive_remote, config$common_lib$rel)))

  our_lib <-
    prepare_path(context:::path_library(path, "windows", config$r_version),
                 config$shares)
  expect_equal(res[[1L]],
               unix_path(file.path(our_lib$drive_remote, our_lib$rel)))
})
