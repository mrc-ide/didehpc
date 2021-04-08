context("config")

test_that("defaults are sensible", {
  res <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config_defaults())
  non_null <- c("cluster", "use_workers", "use_rrq", "worker_timeout",
                "use_java", "conan_bootstrap")
  null <- c("credentials", "home", "temp", "shares", "template", "cores",
            "wholenode", "parallel", "workdir", "r_version", "java_home")
  i <- vlapply(res, is.null)
  expect_setequal(names(res)[i], null)
  expect_setequal(names(res)[!i], non_null)
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
  expect_equal(valid_clusters(), c("fi--dideclusthn", "fi--didemrchnb"))
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
    list(parallel = FALSE, count = 1L, type = "Cores"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, NULL, NULL),
    list(parallel = FALSE, count = 1L, type = "Cores"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, TRUE, FALSE),
    list(parallel = FALSE, count = 1L, type = "Nodes"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, TRUE, TRUE),
    list(parallel = TRUE, count = 1L, type = "Nodes"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", 8, FALSE, FALSE),
    list(parallel = FALSE, count = 8L, type = "Cores"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", 8, FALSE, NULL),
    list(parallel = TRUE, count = 8L, type = "Cores"))
  expect_error(
    check_resources("fi--dideclusthn", "GeneralNodes", 9001, FALSE, FALSE),
    "Maximum number of cores for fi--dideclusthn is 24")
  expect_error(
    check_resources("fi--dideclusthn", "GeneralNodes", 2, TRUE, FALSE),
    "Cannot specify both wholenode and cores")
})


test_that("Can find redis host, given cluster", {
  expect_equal(redis_host("fi--didemrchnb"), "12.0.0.1")
  expect_equal(redis_host("fi--dideclusthn"), "11.0.0.1")
  expect_error(redis_host("fi--didegpu"),
               "No redis host for cluster 'fi--didegpu'")
})


test_that("Can get a reasonable rtools version", {
  expect_equal(
    rtools_versions("<prefix>", numeric_version("4.0.0")),
    list(gcc = "mingw$(WIN)",
         make = "usr",
         binpref = "<prefix>/Rtools/Rtools40/mingw$(WIN)/bin",
         rtools_root = "<prefix>\\Rtools\\Rtools40",
         gcc_path = "<prefix>\\Rtools\\Rtools40\\mingw$(WIN)\\bin",
         make_path = "<prefix>\\Rtools\\Rtools40\\usr\\bin"))
  expect_equal(
    rtools_versions("<prefix>", numeric_version("3.6.3")),
    list(gcc = "mingw_$(WIN)",
         make = "",
         binpref = "<prefix>/Rtools/Rtools35/mingw_$(WIN)/bin",
         rtools_root = "<prefix>\\Rtools\\Rtools35",
         gcc_path = "<prefix>\\Rtools\\Rtools35\\mingw_$(WIN)\\bin",
         make_path = "<prefix>\\Rtools\\Rtools35\\\\bin"))
  expect_equal(
    rtools_versions("<prefix>", numeric_version("3.5.0")),
    rtools_versions("<prefix>", numeric_version("3.6.0")))
  expect_equal(
    rtools_versions("<prefix>", numeric_version("3.4.3")),
    rtools_versions("<prefix>", numeric_version("3.5.0")))
  expect_equal(
    rtools_versions("<prefix>", numeric_version("3.3.3")),
    rtools_versions("<prefix>", numeric_version("3.5.0")))
})


test_that("fetch r versions", {
  ## testthat::skip_if_offline()
  dat <- r_versions()
  expect_is(dat, "numeric_version")
  expect_true(numeric_version("4.0.3") %in% dat)
  expect_true(length(dat) > 10)
})


test_that("Select a sensible r version", {
  ## testthat::skip_if_offline()
  v <- r_versions()
  vmax <- max(v)
  vmid <- v[length(v) - 3]
  expect_equal(select_r_version(vmax), vmax)
  expect_error(select_r_version("3.4.9"),
               "Unsupported R version: 3.4.9")
  expect_equal(select_r_version(NULL, vmid), vmid)
  expect_equal(select_r_version(NULL, "3.4.9"), numeric_version("3.5.0"))
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

  withr::with_options(list(didehpc.credentials = "alice"), {
    res <- didehpc_config_global(credentials = "bob", check = FALSE)
    expect_equal(res$didehpc.credentials, "alice")
    expect_equal(getOption("didehpc.credentials"), "bob")
    expect_error(
      didehpc_config_global(credentials = "charlie", check = TRUE),
      "I can't find your home directory!  Please mount it")
    expect_equal(getOption("didehpc.credentials"), "bob")
  })
})
