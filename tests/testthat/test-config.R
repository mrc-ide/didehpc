context("config")

## Always a hassle.  I should write a package for package configurations...
test_that("globals are not usually defined", {
  expect_null(getOption("didewin.credentials"))
  expect_null(getOption("didewin.home"))
  expect_null(getOption("didewin.temp"))
  expect_null(getOption("didewin.cluster"))
})

test_that("credentials", {
  if (!interactive()) {
    expect_error(get_credentials(), "Credentials file needed")
  }
  expect_equal(get_credentials("username", FALSE),
               list(username="username"))
  expect_equal(get_credentials("DIDE\\username", FALSE),
               list(username="username"))

  ## Lists:
  expect_equal(get_credentials(list(username="username"), FALSE),
               list(username="username"))
  expect_error(get_credentials(list(username="username"), TRUE),
               "Missing fields")
  expect_equal(get_credentials(list(username="username", password="pw"), FALSE),
               list(username="username", password="pw"))
  expect_equal(get_credentials(list(username="username", password="pw"), TRUE),
               list(username="username", password="pw"))

  expect_error(get_credentials(list(username="foo", password="bar", extra="x")),
               "Unknown fields")
  expect_error(get_credentials(list("username")),
               "must be named")
  expect_error(get_credentials(list()),
               "must be named")
  expect_error(get_credentials(list(password="foo")),
               "Missing fields")

  writeLines(c("username=us", "password=pw"), tmp <- tempfile())
  expect_equal(get_credentials(tmp, FALSE),
               list(username="us", password="pw"))
})

test_that("interactive", {
  if (!interactive()) {
    expect_error(didewin_config(), "Credentials file needed")
    expect_error(didewin_config("username"), "Credentials file needed")
  }

  dat <- didewin_config("me")
  expect_equal(dat$credentials, "me")
  expect_equal(dat$username, "me")
  expect_null(dat$password)
  expect_equal(dat$cluster, valid_clusters()[[1]])
  ## This might become an error soon because there will be nothing to
  ## map against.
  expect_equal(dat$shares, list())

  dat <- didewin_config("me", home=tempdir())
  expect_equal(length(dat$shares), 1)

  expect_equal(dat$shares[[1]],
               structure(list(name="home",
                              path_remote=clean_path(dide_home("", "me")),
                              path_local=tempdir(),
                              drive_remote="Q:"),
                         class="path_mapping"))
})

test_that("globals", {
  oo <- didewin_config_global(credentials="foo")
  on.exit(options(oo))
  expect_equal(oo, list(didewin.credentials=NULL))
})
