context("mounts")

test_that("detect_mount uses correct implementation", {
  ## Pretty heavy mocking here!
  mock_is_windows <- mockery::mock(TRUE, FALSE)
  mock_dmw <- mockery::mock()
  mock_dmu <- mockery::mock()

  mockery::stub(detect_mount, "is_windows", mock_is_windows)
  mockery::stub(detect_mount, "detect_mount_windows", mock_dmw)
  mockery::stub(detect_mount, "detect_mount_unix", mock_dmu)

  detect_mount()
  mockery::expect_called(mock_is_windows, 1)
  mockery::expect_called(mock_dmw, 1)
  mockery::expect_called(mock_dmu, 0)

  detect_mount()
  mockery::expect_called(mock_is_windows, 2)
  mockery::expect_called(mock_dmw, 1)
  mockery::expect_called(mock_dmu, 1)
})


test_that("return sensible data when no mounts found", {
  mock_system2 <- mockery::mock(character())
  mockery::stub(detect_mount_unix, "system2", mock_system2)
  res <- detect_mount_unix()
  expect_equal(res, cbind(remote = character(), local = character()))
})


test_that("Parse return value into sensible output", {
  dat <- c(
    "//fi--didef3/other on /home/bob/net/other type cifs (rw,relatime)",
    "//fi--san03/homes/bob on /home/bob/net/home type cifs (rw,relatime)",
    "//fi--didenas1/Malaria on /home/bob/net/malaria type cifs (rw,relatime)")
  mock_system2 <- mockery::mock(dat)
  mockery::stub(detect_mount_unix, "system2", mock_system2)
  res <- detect_mount_unix()
  cmp <- cbind(remote = c("\\\\fi--didef3\\other",
                          "\\\\fi--san03\\homes\\bob",
                          "\\\\fi--didenas1\\Malaria"),
               local = c("/home/bob/net/other",
                         "/home/bob/net/home",
                         "/home/bob/net/malaria"))
  expect_equal(res, cmp)
})


test_that("Warn if given unexpected output", {
  dat <- c(
    "//fi--didef3/other on /home/bob/net/other type cifs (rw,relatime)",
    "//fi--san03/homes/bob sur /home/bob/net/home type cifs (rw,relatime)",
    "//fi--didenas1/Malaria on /home/bob/net/malaria type cifs (rw,relatime)")
  mock_system2 <- mockery::mock(dat)
  mockery::stub(detect_mount_unix, "system2", mock_system2)
  expect_warning(
    res <- detect_mount_unix(),
    "Ignoring mounts")
  cmp <- cbind(remote = c("\\\\fi--didef3\\other",
                          "\\\\fi--didenas1\\Malaria"),
               local = c("/home/bob/net/other",
                         "/home/bob/net/malaria"))
  expect_equal(res, cmp)
})


test_that("Can parse wmic output", {
  x <- c("\r",
         "Node,ConnectionState,LocalName,RemoteName,Status\r",
         "BUILDERHV,Connected,q:,\\\\fi--san03\\homes\\bob,OK\r",
         "BUILDERHV,Connected,T:,\\\\fi--didef3\\tmp,OK\r")
  expect_equal(
    wmic_parse(x),
    cbind(remote = c("\\\\fi--san03\\homes\\bob", "\\\\fi--didef3\\tmp"),
          local = c("q:", "T:")))
})


test_that("Can validate wmic output", {
  x <- c("\r",
         "node,connectionstate,localname,remotename,status\r",
         "BUILDERHV,Connected,q:,\\\\fi--san03\\homes\\bob,OK\r",
         "BUILDERHV,Connected,T:,\\\\fi--didef3\\tmp,OK\r")
  expect_error(
    wmic_parse(x),
    "Failed to find expected names in wmic output: RemoteName, LocalName")
})


test_that("detect_mount_windows tries different methods in turn", {
  err <- list(success = FALSE,
              result = tryCatch(stop("some error"), error = identity))
  res <- list(success = TRUE,
              result = cbind(remote = "\\\\fi--remote\\path", local = "Q:"))
  mock_wmic_call <- mockery::mock(err, res)
  mockery::stub(detect_mount_windows, "wmic_call", mock_wmic_call)

  expect_equal(detect_mount_windows(), res$result)

  mockery::expect_called(mock_wmic_call, 2)
  expect_equal(
    mockery::mock_args(mock_wmic_call),
    list(list("csv"),
         list("C:\\windows\\System32\\wbem\\en-US\\csv")))
})


test_that("detect_mount_windows errors if no method found", {
  err <- list(success = FALSE, result = "some error")
  mock_wmic_call <- mockery::mock(err, cycle = TRUE)
  mockery::stub(detect_mount_windows, "wmic_call", mock_wmic_call)
  expect_error(
    detect_mount_windows(),
    "Could not determine windows mounts using wmic.+some error")
  mockery::expect_called(mock_wmic_call, 3)
  expect_equal(
    mockery::mock_args(mock_wmic_call),
    list(list("csv"),
         list("C:\\windows\\System32\\wbem\\en-US\\csv"),
         list("C:\\windows\\System32\\wbem\\en-GB\\csv")))
})



test_that("wmic_call copes with command and parse errors", {
  res_err <- structure(character(0), status = 1)
  res_bad <- "lolno"
  res_good <- c("\r",
         "Node,ConnectionState,LocalName,RemoteName,Status\r",
         "BUILDERHV,Connected,q:,\\\\fi--san03\\homes\\bob,OK\r",
         "BUILDERHV,Connected,T:,\\\\fi--didef3\\tmp,OK\r")

  mock_system <- mockery::mock(stop("Error running command"), res_bad, res_good)
  mockery::stub(wmic_call, "system_internal_check", mock_system)

  res1 <- wmic_call("csv")
  res2 <- wmic_call("csv")
  res3 <- wmic_call("csv")

  expect_equal(
    res1,
    list(success = FALSE, result = "Error running command"))
  expect_equal(
    res2,
    list(
      success = FALSE,
      result = paste("Failed to find expected names in wmic output:",
                     "RemoteName, LocalName")))
  expect_equal(
    res3,
    list(success = TRUE, result = wmic_parse(res_good)))

  mockery::expect_called(mock_system, 3)
  expect_equal(
    mockery::mock_args(mock_system),
    rep(list(list('wmic netuse list brief /format:"csv"')), 3))
})
