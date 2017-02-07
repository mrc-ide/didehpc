context("compiled dependency")

test_that("compiled dependency", {
  skip_on_travis()
  skip_if_not_installed("buildr")

  owd <- prepare_didewin("compiled")
  on.exit(setwd(owd))

  path <- "context"
  src <- provisionr::package_sources(github = "richfitz/seagull")
  ctx <- context::context_save(path = path, packages = "seagull",
                               package_sources = src)
  obj <- didewin::queue_didewin(ctx)

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  expect_equal(names(res$otherPkgs), "seagull")

  msg <- capture_messages(obj <- didewin::queue_didewin(ctx))
  expect_false(any(grepl("drat", msg)))
})

## This simulates someone manually compiling packages and inserting
## them into the drat as _binaries_ for windows.
test_that("manual compilation", {
  skip_on_travis()
  skip_if_not_installed("buildr")

  owd <- prepare_didewin("compiled-manual")
  on.exit(setwd(owd))

  path <- "context"
  src <- provisionr::package_sources(github = "richfitz/seagull")
  src$build()

  contrib <- file.path(src$local_drat, "src", "contrib")
  p <- read.dcf(file.path(contrib, "PACKAGES"))[1L, , drop = TRUE]

  pkg <-
    file.path(contrib, sprintf("%s_%s.tar.gz", p[["Package"]], p[["Version"]]))
  bin <- buildr::build_binaries(pkg, BUILD_SERVER_WINDOWS, 8732)

  src2 <- provisionr::package_sources(local = bin)
  src2$build()

  ctx <- context::context_save(path = path, packages = "seagull",
                               package_sources = src2)
  obj <- didewin::queue_didewin(ctx)

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  expect_equal(names(res$otherPkgs), "seagull")

  msg <- capture_messages(obj <- didewin::queue_didewin(ctx))
  expect_false(any(grepl("drat", msg)))
})
