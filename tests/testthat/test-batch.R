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
    grep("{{", strsplit(res$conan, "\n")[[1]], fixed = TRUE), 1)
  expect_length(
    grep("{{", strsplit(res$runner, "\n")[[1]], fixed = TRUE), 1)
  expect_length(
    grep("{{", strsplit(res$rrq_worker, "\n")[[1]], fixed = TRUE), 2)
})


test_that("Can create path and template data", {
  config <- example_config()
  root <- file.path(config$workdir, "context")
  dir.create(root, FALSE, TRUE)
  context_id <- ids::random_id()
  dat <- batch_data(root, context_id, config)
  expect_setequal(names(dat), c("templates", "paths"))
  expect_equal(dat$templates,
               batch_templates(root, context_id, config, config$workdir))
  expect_setequal(names(dat$paths), c("local", "remote"))
  expect_equal(names(dat$paths$remote), names(dat$paths$local))

  expect_setequal(
    names(dat$paths$local),
    c("root", "conan", "batch", "lib", "log", "worker_log", "workdir"))
})


test_that("batch data creates entries for share drives", {
  config <- example_config(r_version = numeric_version("4.0.3"))
  root <- file.path(config$workdir, "context")
  dir.create(root, FALSE, TRUE)
  context_id <- ids::random_id()
  dat <- template_data(root, context_id, config, config$workdir)
  expect_match(dat$network_shares_create,
               "net use Q:", fixed = TRUE)
  expect_match(dat$network_shares_create,
               "net use T:", fixed = TRUE)
})


test_that("can disable conan bootstrap", {
  config <- example_config(r_version = numeric_version("4.0.3"))
  root <- file.path(config$workdir, "context")
  dir.create(root, FALSE, TRUE)
  context_id <- ids::random_id()
  dat1 <- template_data(root, context_id, config, config$workdir)
  config$conan_bootstrap <- FALSE
  dat2 <- template_data(root, context_id, config, config$workdir)

  expect_equal(dat1$conan_path_bootstrap, "T:\\conan\\bootstrap\\4.0")
  expect_null(dat2$conan_path_bootstrap)
  expect_equal(dat1[names(dat1) != "conan_path_bootstrap"],
               dat2[names(dat2) != "conan_path_bootstrap"])
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


test_that("Parallel sets CONTEXT_CORES", {
  config <- example_config()
  root <- file.path(config$workdir, "context")
  dir.create(root, FALSE, TRUE)
  context_id <- ids::random_id()
  dat1 <- template_data(root, context_id, config, config$workdir)

  config <- example_config(cores = 10,
                           root = dirname(config$shares$home$path_local))
  dat2 <- template_data(root, context_id, config, config$workdir)

  expect_null(dat1$parallel)
  expect_match(dat2$parallel, "CONTEXT_CORES=%CCP_NUMCPUS%")
  expect_equal(dat1[names(dat1) != "parallel"], dat2[names(dat2) != "parallel"])
})


test_that("Configure different workers and controllers", {
  w <- worker_resource(cores = 8, parallel = FALSE, template = "8Core")
  config <- example_config(cores = 4, parallel = TRUE, worker_resource = w,
                           use_rrq = TRUE)
  root <- file.path(config$workdir, "context")
  dir.create(root, FALSE, TRUE)
  context_id <- ids::random_id()
  res <- batch_templates(root, context_id, config, config$workdir)

  expect_length(grep("set CONTEXT_CORES=%CCP_NUMCPUS%", res$runner), 1)
  expect_length(grep("set CONTEXT_CORES=%CCP_NUMCPUS%", res$rrq_worker), 0)
})
