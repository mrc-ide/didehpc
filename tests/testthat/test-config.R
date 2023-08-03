context("config")

test_that("defaults are sensible", {
  res <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config_defaults())
  non_null <- c("cluster", "use_workers", "use_rrq", "worker_timeout",
                "use_java", "conan_bootstrap")
  null <- c("home", "temp", "shares", "template", "cores",
            "wholenode", "parallel", "workdir", "r_version", "java_home")
  i <- vlapply(res, is.null)
  expect_true("credentials" %in% names(res)) # might be here or not
  expect_setequal(setdiff(names(res)[i], "credentials"), null)
  expect_setequal(setdiff(names(res)[!i], "credentials"), non_null)
  expect_equal(res$cluster, "fi--dideclusthn") # old cluster
  expect_false(res$use_workers)
  expect_false(res$use_rrq)
  expect_equal(res$worker_timeout, 600)
  expect_true(res$conan_bootstrap)
  expect_false(res$use_java)
})

test_that("use didehpc.username if needed and available", {
  blank <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config_defaults())
  res1 <- withr::with_options(
    tmp_options_didehpc(didehpc.credentials = "bob"),
    didehpc_config_defaults())
  res2 <- withr::with_options(
    tmp_options_didehpc(didehpc.username = "bob"),
    didehpc_config_defaults())
  res3 <- withr::with_options(
    tmp_options_didehpc(didehpc.username = "alice",
                        didehpc.credentials = "bob"),
    didehpc_config_defaults())

  expect_equal(res1$credentials, "bob")
  expect_mapequal(res1[names(res1) != "credentials"],
                  res2[names(res2) != "credentials"])
  expect_equal(res2, res1)
  expect_equal(res3, res1)
})


test_that("Try and get the username out of windows", {
  mock_is_windows <- mockery::mock(FALSE, TRUE)
  mockery::stub(didehpc_config_defaults, "is_windows", mock_is_windows)

  withr::with_envvar(c(USERNAME = "bob"), {
    res_other <- withr::with_options(
      tmp_options_didehpc(),
      didehpc_config_defaults())
    res_win <- withr::with_options(
      tmp_options_didehpc(),
      didehpc_config_defaults())
  })
  expect_null(res_other$credentials)
  expect_equal(res_win$credentials, "bob")
})


test_that("valid clusters is correct", {
  expect_equal(valid_clusters(), c("fi--dideclusthn", "fi--didemrchnb", 
                                   "wpia-hn"))
})


test_that("Can transform cluster names", {
  expect_equal(cluster_name("fi--dideclusthn"), "fi--dideclusthn")
  expect_equal(cluster_name("small"), "fi--dideclusthn")
  expect_equal(cluster_name("little"), "fi--dideclusthn")
  expect_equal(cluster_name("dide"), "fi--dideclusthn")
  expect_equal(cluster_name("ide"), "fi--dideclusthn")
  expect_equal(cluster_name("dideclusthn"), "fi--dideclusthn")

  expect_equal(cluster_name("fi--didemrchnb"), "fi--didemrchnb")
  expect_equal(cluster_name("big"), "fi--didemrchnb")
  expect_equal(cluster_name("mrc"), "fi--didemrchnb")
  
})


test_that("Check that resources are acceptable", {
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", 1, FALSE, FALSE),
    list(template = "GeneralNodes", parallel = FALSE, count = 1L,
         type = "Cores"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, NULL, NULL),
    list(template = "GeneralNodes", parallel = FALSE, count = 1L,
         type = "Cores"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, TRUE, FALSE),
    list(template = "GeneralNodes", parallel = FALSE, count = 1L,
         type = "Nodes"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, TRUE, TRUE),
    list(template = "GeneralNodes", parallel = TRUE, count = 1L,
         type = "Nodes"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", 8, FALSE, FALSE),
    list(template = "GeneralNodes", parallel = FALSE, count = 8L,
         type = "Cores"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", 8, FALSE, NULL),
    list(template = "GeneralNodes", parallel = TRUE, count = 8L,
         type = "Cores"))
  expect_error(
    check_resources("fi--dideclusthn", "GeneralNodes", 9001, FALSE, FALSE),
    "Maximum number of cores for fi--dideclusthn is 24")
  expect_error(
    check_resources("fi--didemrchnb", "GeneralNodes", 9001, FALSE, FALSE),
    "Maximum number of cores for fi--didemrchnb is 64")
  
  expect_error(
    check_resources("fi--dideclusthn", "GeneralNodes", 2, TRUE, FALSE),
    "Cannot specify both wholenode and cores")
  expect_error(
    didehpc_check_max_cores("unknown_cluster", 99),
    "Invalid cluster 'unknown_cluster'")
})

test_that("default core/node choice consistent across clusters", {
  matching <- function(returned, expected) {
    for (field in c("parallel", "count", "type")) {
      if (!(identical(expected[[field]], returned[[field]]))) {
        return(FALSE)
      }
    }
    TRUE
  }
  
  # Args for check_resources: cluster, template, cores, wholenode, parallel
  
  # If we don't specify cores/parallel/whole node, we should get 1 core.
  
  expect_true(matching(check_resources("fi--didemrchnb", "GeneralNodes", NULL, NULL, NULL),
                       list(parallel = FALSE, count = 1L, type = "Cores")))
  expect_true(matching(check_resources("fi--didemrchnb", "32Core", NULL, NULL, NULL),
                       list(parallel = FALSE, count = 1L, type = "Cores")))
  expect_true(matching(check_resources("wpia-hn", "AllNodes", NULL, NULL, NULL),
                       list(parallel = FALSE, count = 1L, type = "Cores")))
  
})


test_that("Can find redis host, given cluster", {
  expect_equal(redis_host("fi--didemrchnb"), "12.0.0.1")
  expect_equal(redis_host("fi--dideclusthn"), "11.0.0.1")
  expect_equal(redis_host("wpia-hn"), "10.0.2.254")
  expect_error(redis_host("fi--didegpu"),
               "No redis host for cluster 'fi--didegpu'")
})


test_that("fetch r versions", {
  testthat::skip_if_offline()
  dat <- r_versions()
  expect_is(dat, "numeric_version")
  expect_true(numeric_version("4.3.0") %in% dat)
  expect_true(length(dat) > 3)
})


test_that("Select a sensible r version", {
  testthat::skip_if_offline()
  v <- r_versions()
  vmax <- max(v)
  vmid <- v[length(v) - 3]
  expect_equal(select_r_version(vmax), vmax)
  expect_error(select_r_version("3.6.0"),
               "Unsupported R version: 3.6.0")
  expect_equal(select_r_version(NULL, vmid), vmid)
  expect_equal(select_r_version(NULL, "4.1.0"), numeric_version("4.1.3"))
})


test_that("Build config", {
  root <- tempfile()
  mounts <- example_mounts(root)
  workdir <- file.path(root, "home", "sub")
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)
  cfg <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config(credentials = list(username = "bob", password = "secret"),
                   workdir = workdir))
  mockery::expect_called(mock_detect_mount, 1L)
  expect_equal(
    mockery::mock_args(mock_detect_mount), list(list()))

  expect_s3_class(cfg, "didehpc_config")
  str <- capture.output(print(cfg))
  expect_match(str, "<didehpc_config>", all = FALSE)
  expect_match(str, " - username: bob", all = FALSE)
  expect_match(str, " - password: \\*+$", all = FALSE)
  expect_match(str, "    - parallel: FALSE", all = FALSE)
})


test_that("Build config", {
  root <- tempfile()
  mounts <- example_mounts(root)
  workdir <- file.path(root, "home", "sub")
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)
  cfg <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config(credentials = "bob", workdir = workdir))
  mockery::expect_called(mock_detect_mount, 1L)
  expect_equal(
    mockery::mock_args(mock_detect_mount), list(list()))

  expect_s3_class(cfg, "didehpc_config")
  str <- capture.output(print(cfg))
  expect_match(str, "<didehpc_config>", all = FALSE)
  expect_match(str, " - username: bob", all = FALSE)
  expect_match(str, "    - parallel: FALSE", all = FALSE)
})


test_that("workdir must exist", {
  root <- tempfile()
  mounts <- example_mounts(root)
  workdir <- file.path(root, "home", "sub")
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)
  withr::with_options(
    tmp_options_didehpc(),
    expect_error(
      didehpc_config(credentials = "bob", workdir = tempfile()),
      "workdir must be an existing directory"))
})


test_that("config getter requires sensible args", {
  expect_error(
    as_didehpc_config("bob"),
    "Expected a didehpc_config for 'config'")
})


test_that("config getter tries to construct options", {
  root <- tempfile()
  workdir <- file.path(root, "home", "sub")
  config <- example_config()
  mock_do_call <- mockery::mock(config)
  mockery::stub(as_didehpc_config, "do.call", mock_do_call)
  cmp <- withr::with_options(
    tmp_options_didehpc(),
    as_didehpc_config(list(credentials = "bob", workdir = workdir)))
  expect_equal(cmp, config)
  mockery::expect_called(mock_do_call, 1L)
  expect_equal(
    mockery::mock_args(mock_do_call)[[1]],
    list("didehpc_config", list(credentials = "bob", workdir = workdir)))
})


test_that("java options", {
  root <- tempfile()
  cfg1 <- example_config(root = tempfile())
  expect_false(cfg1$use_java)
  expect_null(cfg1$java_home)

  cfg2 <- example_config(use_java = TRUE, root = tempfile())
  expect_true(cfg2$use_java)
  expect_equal(cfg2$java_home, "")

  cfg3 <- example_config(use_java = TRUE, java_home = "T:/java",
                         root = tempfile())
  expect_true(cfg3$use_java)
  expect_equal(cfg3$java_home, "T:/java")
})


test_that("Global options", {
  opts <- options()
  on.exit(options(opts))
  expect_equal(didehpc_config_global(), list())
  expect_error(didehpc_config_global("user"),
               "All options must be named")
  expect_error(didehpc_config_global(credentials = "user", unknown = 2),
               "Unknown options: unknown")
  expect_equal(options(), opts)

  mock_config <- mockery::mock(stop("Some error"))
  mockery::stub(didehpc_config_global, "didehpc_config", mock_config)

  withr::with_options(list(didehpc.credentials = "alice"), {
    res <- didehpc_config_global(credentials = "bob", check = FALSE)
    expect_equal(res$didehpc.credentials, "alice")
    expect_equal(getOption("didehpc.credentials"), "bob")
    mockery::expect_called(mock_config, 0)
    expect_error(
      didehpc_config_global(credentials = "charlie", check = TRUE),
      "Some error")
    mockery::expect_called(mock_config, 1)
    expect_equal(mockery::mock_args(mock_config)[[1]], list())
    expect_equal(getOption("didehpc.credentials"), "bob")
  })
})


test_that("different worker configuration", {
  w <- worker_resource(wholenode = TRUE, template = "8Core",
                       parallel = FALSE)
  expect_is(w, "worker_resource")
  cfg <- example_config(worker_resource = w, use_rrq = TRUE)
  expect_equal(cfg$worker_resource,
               list(template = "8Core", parallel = FALSE, count = 1,
                    type = "Nodes"))
})


test_that("different worker configuration requires use_rrq", {
  w <- worker_resource(wholenode = TRUE, template = "8Core",
                       parallel = FALSE)
  expect_error(
    example_config(worker_resource = w),
    "'worker_resource' provided but 'use_rrq' is FALSE")
})


test_that("error in worker configuration raises informatively", {
  expect_error(
    example_config(worker_resource = worker_resource(cores = 9001),
                   use_rrq = TRUE),
    "Invalid worker resource request: Maximum number of cores for")
})
