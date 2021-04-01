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
