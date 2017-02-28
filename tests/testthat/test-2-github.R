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
