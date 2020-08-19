context("config")

skip_on_windows <- function() {
  testthat::skip_on_os("windows")
}

test_that("credentials", {
  if (!interactive()) {
    expect_error(get_credentials(NULL), "Credentials file needed")
    expect_error(get_credentials("username", TRUE), "Credentials file needed")
  }
  expect_equal(get_credentials("username", FALSE),
               list(username = "username"))
  expect_equal(get_credentials("DIDE\\username", FALSE),
               list(username = "username"))

  ## Lists:
  expect_equal(get_credentials(list(username = "username"), FALSE),
               list(username = "username"))
  expect_error(get_credentials(list(username = "username"), TRUE),
               "Missing fields")
  expect_equal(
    get_credentials(list(username = "username", password = "pw"), FALSE),
    list(username = "username", password = "pw"))
  expect_equal(
    get_credentials(list(username = "username", password = "pw"), TRUE),
    list(username = "username", password = "pw"))

  expect_error(
    get_credentials(list(username = "foo", password = "bar", extra = "x")),
               "Unknown fields")
  expect_error(get_credentials(list("username")),
               "must be named")
  expect_error(get_credentials(list()),
               "must be named")
  expect_error(get_credentials(list(password = "foo")),
               "Missing fields")

  writeLines(c("username = us", "password = pw"), tmp <- tempfile())
  expect_equal(get_credentials(tmp, FALSE),
               list(username = "us", password = "pw"))
})

test_that("interactive", {
  skip_on_windows()
  if (!interactive()) {
    expect_error(didehpc_config(), "Credentials file needed")
  }

  dat <- didehpc_config("me")
  expect_equal(dat$credentials, "me")
  expect_equal(dat$username, "me")
  expect_null(dat$password)
  ## This might become an error soon because there will be nothing to
  ## map against.
  expect_is(dat$shares, "list")

  dat <- didehpc_config("me", home = tempdir())
  
  tempdir_slashes <- normalizePath(tempdir(), winslash = "/")

  expect_equal(dat$shares$home,
               structure(list(name = "home",
                              path_remote = clean_path(dide_home("", "me")),
                              path_local = tempdir_slashes,
                              drive_remote = "Q:"),
                         class = "path_mapping"))
})

test_that("unset globals", {
  oo <- options()
  on.exit(options(oo))

  didehpc_config_global(credentials = "foo")
  expect_equal(getOption("didehpc.credentials"), "foo")

  didehpc_config_global(credentials = NULL)
  expect_equal(getOption("didehpc.credentials"), NULL)
  expect_equal(getOption("didehpc.credentials", NA), NA) # really not set
})

test_that("template logic", {
  oo <- didehpc_config_global(credentials = "foo", cluster = "fi--dideclusthn")
  on.exit(options(oo))

  expect_equal(didehpc_config()$resource,
               list(parallel = FALSE, count = 1, type = "Cores"))
  expect_equal(didehpc_config(wholenode = TRUE)$resource,
               list(parallel = TRUE, count = 1, type = "Nodes"))
  expect_equal(didehpc_config(wholenode = TRUE, parallel = TRUE)$resource,
               list(parallel = TRUE, count = 1, type = "Nodes"))
  expect_equal(didehpc_config(wholenode = TRUE, parallel = FALSE)$resource,
               list(parallel = FALSE, count = 1, type = "Nodes"))

  expect_error(didehpc_config(wholenode = TRUE, cores = 2)$resource,
               "Cannot specify both wholenode and cores")
  
  expect_error(didehpc_config(cores = 999)$resource,
               "Maximum number of cores for")

  expect_equal(didehpc_config(template = "8Core")$resource,
               list(parallel = TRUE, count = 1, type = "Nodes"))
  expect_equal(didehpc_config(template = "8Core", parallel = FALSE)$resource,
               list(parallel = FALSE, count = 1, type = "Nodes"))
  expect_equal(didehpc_config(template = "8Core", cores = 3)$resource,
               list(parallel = TRUE, count = 3, type = "Cores"))
})

test_that("parallel and cores", {
  oo <- didehpc_config_global(credentials = "foo")
  on.exit(options(oo))
  expect_false(didehpc_config()$resource$parallel)
  expect_false(didehpc_config(cores = 1)$resource$parallel)
  expect_true(didehpc_config(cores = 2)$resource$parallel)
})

test_that("parallel and cores with parallel given", {
  oo <- didehpc_config_global(credentials = "foo")
  on.exit(options(oo))
  expect_true(didehpc_config(parallel = TRUE, cores = 8)$resource$parallel)
  expect_true(didehpc_config(parallel = NULL, cores = 8)$resource$parallel)
  expect_false(didehpc_config(parallel = FALSE, cores = 8)$resource$parallel)

  expect_true(
    didehpc_config(parallel = TRUE, template = "16Core")$resource$parallel)
  expect_true(
    didehpc_config(parallel = NULL, template = "16Core")$resource$parallel)
  expect_false(
    didehpc_config(parallel = FALSE, template = "16Core")$resource$parallel)
})

test_that("cluster alias", {
  expect_equal(didehpc_config(cluster = "big")$cluster, "fi--didemrchnb")
  expect_equal(didehpc_config(cluster = "linux")$cluster, "fi--didelxhn")
  expect_equal(didehpc_config(cluster = "SMALL")$cluster, "fi--dideclusthn")
})

test_that("old home directory", {
  skip_on_windows()
  dat <- cbind(remote = c("\\\\fi--san03\\homes\\me",
                          "\\\\fi--didef3\\tmp"),
               local = c(tempfile(), tempfile()))
  for (p in dat[, "local"]) {
    dir.create(p)
  }

  ## First version is OK
  expect_warning(
    testthat::with_mock(
      `didehpc:::detect_mount` = function() dat,
      dide_detect_mount(NULL, NULL, NULL, NULL, "me", "fi--didemrchnb", TRUE)),
    NA)

  ## old location
  dat[1, "remote"] <- "\\\\fi--san02\\homes\\me"
  expect_warning(
    testthat::with_mock(
      `didehpc:::detect_mount` = function() dat,
      dide_detect_mount(NULL, NULL, NULL, NULL, "me", "fi--didemrchnb", TRUE)),
    "to point at the new network location")

  ## two copies of home (this is close to what Marga had)
  dat[2, "remote"] <- "\\\\fi--san03\\HOMES\\other"
  expect_error(
    testthat::with_mock(
      `didehpc:::detect_mount` = function() dat,
      dide_detect_mount(NULL, NULL, NULL, NULL, "me", "fi--didemrchnb", TRUE)),
    "I am confused about your home directory; there are 2 choices")
})
