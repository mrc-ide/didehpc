context("batch")

test_that("can load all templates", {
  d <- read_templates()
  expect_setequal(
    names(d),
    c("conan", "runner", "rrq_worker"))
  ## Protects against incorrect pasting of templates
  expect_true(all(vlapply(d, grepl, pattern = "Quitting$")))
})


test_that("Create templates", {
  config <- example_config()
  root <- file.path(config$workdir, "context")
  dir.create(root, FALSE, TRUE)
  context_id <- ids::random_id()
  res <- batch_templates(root, context_id, config, config$workdir)

  expect_length(
    grep("{{{", strsplit(res$conan, "\n")[[1]], fixed = TRUE), 1)
  expect_length(
    grep("{{{", strsplit(res$runner, "\n")[[1]], fixed = TRUE), 1)
  expect_length(
    grep("{{{", strsplit(res$rrq_worker, "\n")[[1]], fixed = TRUE), 2)
})


test_that("can disable conan cache", {
  config <- example_config()
  root <- file.path(config$workdir, "context")
  dir.create(root, FALSE, TRUE)
  context_id <- ids::random_id()
  dat1 <- template_data(root, context_id, config, config$workdir)
  config$conan_cache <- FALSE
  dat2 <- template_data(root, context_id, config, config$workdir)

  expect_equal(dat1$conan_root, "T:\\conan\\4.0")
  expect_null(dat2$conan_root)
  expect_equal(dat1[names(dat1) != "conan_root"],
               dat2[names(dat2) != "conan_root"])
})


test_that("can create temp drive if not listed cache", {
  config <- example_config()
  root <- file.path(config$workdir, "context")
  dir.create(root, FALSE, TRUE)
  context_id <- ids::random_id()
  dat1 <- template_data(root, context_id, config, config$workdir)

  config$shares <- config$shares["home"]
  dat2 <- template_data(root, context_id, config, config$workdir)
  expect_equal(dat1, dat2)
})