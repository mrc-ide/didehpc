context("github")

test_that("github", {
  skip_on_travis()
  skip_if_not_installed("kitten")

  owd <- prepare_didehpc("github")
  on.exit(setwd(owd))

  path <- "context"
  src <- provisionr::package_sources(github = "richfitz/kitten")
  ctx <- context::context_save(path = path, packages = "kitten",
                               package_sources = src)
  obj <- didehpc::queue_didehpc(ctx)

  expect_true(obj$provisioned)
  expect_silent(obj$preflight())

  ## TODO: this is not a great way of determining this:
  path_lib <- context:::path_library(path, "windows", obj$config$r_version)

  ## Check that our kitten package is found:
  expect_true("kitten" %in% dir(path_lib))

  ## Check that it is loaded:
  t1 <- obj$enqueue(sessionInfo())
  res1 <- t1$wait(10, progress = FALSE)
  expect_equal(names(res1$otherPkgs), "kitten")

  ## Then use the package:
  dest <- "kitten.png"
  t2 <- obj$enqueue(kitten::kitten(200, 200, dest))
  res2 <- t2$wait(10, progress = FALSE)
  expect_equal(res2, dest)
  expect_true(file.exists(dest))
})

test_that("upgrade", {
  skip_on_travis()

  lib <- tempfile()
  install.packages("hello", repos = NULL, lib.loc = lib)
  owd <- prepare_didehpc("github", "hello")
  on.exit({
    tryCatch(detach("package:hello"), error = function(e) NULL)
    unlink(lib, recursive = TRUE)
    setwd(owd)
  })

  path <- "context"
  src <- provisionr::package_sources(local = "hello")

  ctx <- context::context_save(path = path,
                               packages = "hello",
                               package_sources = src)
  obj <- didehpc::queue_didehpc(ctx, config = list(use_common_lib = COMMON))

  ## OK, now we get to this point I need to be able to provide an
  ## interface for clearing out packages, etc.  It might be nice to
  ## include this information in the provisioning and add it into the
  ## general data returned by the queue object?
  path_lib_win <- context:::path_library(path, "windows", obj$config$r_version)
  path_drat <- file.path(path, "drat")

  vi <- read_version(file.path(path_lib_win, "hello"))
  v0 <- read_version("hello")
  v1 <- alter_package_version("hello", TRUE)

  ## No change:
  obj$provision()
  expect_identical(read_version(file.path(path_lib_win, "hello")), v0)

  ## Don't update the drat, so no change
  obj$provision(installed_action = "upgrade")
  expect_identical(read_version(file.path(path_lib_win, "hello")), v0)

  ## Finally, update the drat
  obj$provision(installed_action = "upgrade", refresh_drat = TRUE)
  expect_identical(read_version(file.path(path_lib_win, "hello")), v1)
})
