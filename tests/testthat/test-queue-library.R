context("queue_library")

test_that("Create queue library", {
  dide_id <- "824612"
  client <- list(
    login = mockery::mock(),
    submit = mockery::mock(dide_id),
    status_job = mockery::mock("PENDING", "RUNNING", "COMPLETE"))

  cluster <- "fi--didemrchnb"
  config <- example_config()
  root <- file.path(config$workdir, "context")
  ctx <- context::context_save(root)
  data <- batch_data(root, ctx$id, config)

  obj <- queue_library$new(data, cluster, client)
  expect_s3_class(obj, "queue_library")

  expected <- list(complete = FALSE, found = character(0), missing = "context")
  expect_equal(
    obj$check("context"),
    expected)

  ## Quick interaction check as this one just passes through
  mock_conan_check <- mockery::mock(expected)
  mockery::stub(obj$check, "conan::conan_check", mock_conan_check)
  expect_equal(obj$check("context"), expected)
  mockery::expect_called(mock_conan_check, 1L)
  expect_equal(mockery::mock_args(mock_conan_check)[[1]],
               list("context", data$paths$local$lib))

  ## Really nasty interaction test here:
  expect_equal(
    obj$provision("context", show_progress = FALSE, show_log = FALSE, poll = 0),
    "COMPLETE")

  expect_length(dir(file.path(data$paths$local$conan, "bin")), 1)
  id <- dir(file.path(data$paths$local$conan, "bin"))
  path_batch_local <-
    file.path(data$paths$local$conan, "batch", paste0(id, ".bat"))
  expect_true(file.exists(path_batch_local))
  expect_true(paste0("set CONAN_ID=", id) %in% readLines(path_batch_local))

  mockery::expect_called(client$login, 1L)
  mockery::expect_called(client$submit, 1L)
  args <- mockery::mock_args(client$submit)[[1]]
  path_batch <- windows_path(file.path(data$paths$remote$conan, "batch",
                                       paste0(id, ".bat")))
  expect_equal(
    args,
    list(path_batch, paste0("conan:", id), "BuildQueue", cluster))

  mockery::expect_called(client$status_job, 3)
  expect_equal(
    mockery::mock_args(client$status_job),
    rep(list(list(dide_id, cluster)), 3))
})


test_that("provisioning selects appropriate queue", {
  expect_equal(queue_template("fi--didemrchnb"), "BuildQueue")
  expect_equal(queue_template("fi--dideclusthn"), "GeneralNodes")
})


test_that("provisioning policy logic", {
  expect_equal(provision_policy(TRUE), "lazy")
  expect_equal(provision_policy(FALSE), "later")
  expect_equal(provision_policy("lazy"), "lazy")
  expect_error(provision_policy("other", "policy"),
               "'policy' must be one of 'lazy', 'upgrade', 'later', 'fake")
})
