context("compiled dependency")

test_that("compiled dependency", {
  skip_on_travis()
  skip_if_not_installed("buildr")

  owd <- prepare_didehpc("compiled")
  on.exit(setwd(owd))

  path <- "context"
  src <- provisionr::package_sources(github = "richfitz/seagull")
  ctx <- context::context_save(path = path, packages = "seagull",
                               package_sources = src)
  obj <- didehpc::queue_didehpc(ctx)

  lib <- context:::path_library(path, "windows", obj$config$r_version)
  expect_true("seagull" %in% dir(lib))

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  expect_equal(names(res$otherPkgs), "seagull")

  msg <- capture_messages(obj <- didehpc::queue_didehpc(ctx))
  expect_false(any(grepl("drat", msg)))
})

## This simulates someone manually compiling packages and inserting
## them into the drat as _binaries_ for windows.
test_that("manual compilation", {
  skip_on_travis()
  skip_if_not_installed("buildr")

  owd <- prepare_didehpc("compiled-manual")
  on.exit(setwd(owd))

  path <- "context"
  src <- provisionr::package_sources(github = "richfitz/seagull")
  src$build()

  contrib <- file.path(src$local_drat, "src", "contrib")
  p <- read.dcf(file.path(contrib, "PACKAGES"))[1L, , drop = TRUE]

  pkg <-
    file.path(contrib, sprintf("%s_%s.tar.gz", p[["Package"]], p[["Version"]]))
  bin <- buildr::build_binaries(pkg, BUILD_SERVER_WINDOWS, BUILD_SERVER_PORT)

  src2 <- provisionr::package_sources(local = bin)
  src2$build()

  ctx <- context::context_save(path = path, packages = "seagull",
                               package_sources = src2)
  obj <- didehpc::queue_didehpc(ctx)

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  expect_equal(names(res$otherPkgs), "seagull")

  msg <- capture_messages(obj <- didehpc::queue_didehpc(ctx))
  expect_false(any(grepl("drat", msg)))
})

test_that("update binary", {
  ## This is a _giant_ faff because I need to pull down and _extract_
  ## seagull.
  skip_on_travis()
  skip_if_not_installed("buildr")

  owd <- prepare_didehpc("compiled")
  on.exit(setwd(owd))

  src <- provisionr::package_sources(github = "richfitz/seagull")
  tmp <- src$clone()$build()
  file <- dir(file.path(tmp$local_drat, "src", "contrib"),
              pattern = "^seagull_.*\\.tar\\.gz$", full.names = TRUE)
  provisionr:::drat_storr(tmp$local_drat)$get("github::richfitz/seagull")
  file <- dir(file.path(tmp$local_drat, "src", "contrib"),
              pattern = "^seagull_.*\\.tar\\.gz$", full.names = TRUE)
  untar(file, exdir = ".")
  seagull <- "seagull"

  v0 <- read_version(seagull)

  path <- "context"
  src <- provisionr::package_sources(local = seagull)
  ctx <- context::context_save(path,
                               packages = "seagull",
                               package_sources = src)
  obj <- didehpc::queue_didehpc(ctx, config = list(use_common_lib = COMMON))

  path_lib_win <- context:::path_library(path, "windows", obj$config$r_version)
  path_drat <- file.path(path, "drat")

  vi <- read_version(file.path(path_lib_win, "seagull"))
  expect_identical(vi, v0)

  ## Then update things:
  v1 <- alter_package_version(seagull, TRUE)
  obj$provision()
  expect_identical(read_version(file.path(path_lib_win, "seagull")), v0)

  obj$provision(refresh_drat = TRUE)
  expect_identical(read_version(file.path(path_lib_win, "seagull")), v1)
})
