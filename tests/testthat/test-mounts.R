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


test_that("return sensible data when no mounts found (linux)", {
  skip_on_os("windows")
  mock_system2 <- mockery::mock(character())
  mockery::stub(detect_mount_unix, "system2", mock_system2)
  res <- detect_mount_unix()
  expect_equal(res, cbind(remote = character(), local = character()))
})

test_that("Parse return value into sensible output (linux)", {
  skip_on_os("windows")
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

test_that("Warn if given unexpected output (linux)", {
  skip_on_os("windows")
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
  win_dir <- Sys.getenv("windir", "C:\\Windows")
  mockery::expect_called(mock_wmic_call, 2)
  expect_equal(
    mockery::mock_args(mock_wmic_call),
    list(list("csv"),
         list(sprintf("%s\\System32\\wbem\\en-US\\csv", win_dir))))
})


test_that("detect_mount_windows errors if no method found", {
  err <- list(success = FALSE, result = "some error")
  mock_wmic_call <- mockery::mock(err, cycle = TRUE)
  mockery::stub(detect_mount_windows, "wmic_call", mock_wmic_call)
  expect_error(
    detect_mount_windows(),
    "Could not determine windows mounts using wmic.+some error")
  mockery::expect_called(mock_wmic_call, 3)
  win_dir <- Sys.getenv("windir", "C:\\Windows")
  expect_equal(
    mockery::mock_args(mock_wmic_call),
    list(list("csv"),
         list(sprintf("%s\\System32\\wbem\\en-US\\csv", win_dir)),
         list(sprintf("%s\\System32\\wbem\\en-GB\\csv", win_dir))))
})



test_that("wmic_call copes with command and parse errors", {
  res_err <- structure(character(0), status = 1)
  res_bad <- "lolno"
  res_good <- c("\r",
         "Node,ConnectionState,LocalName,RemoteName,Status\r",
         "BUILDERHV,Connected,q:,\\\\fi--san03\\homes\\bob,OK\r",
         "BUILDERHV,Connected,T:,\\\\fi--didef3\\tmp,OK\r")

  mock_system <- mockery::mock(stop("Error running command"), res_bad, res_good)
  mockery::stub(wmic_call, "system_intern_check", mock_system)

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


test_that("Can auto-detect home", {
  root <- tempfile()
  dir.create(root, FALSE, TRUE)
  root <- normalizePath(root, mustWork = TRUE)
  mounts <- example_mounts(root)
  res <- dide_detect_mount_home(NULL, mounts, "bob")
  expect_equal(res$name, "home")
  expect_true(same_path(res$path_local, file.path(root, "home")))
  expect_equal(res$path_remote, "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob")
  expect_equal(res$drive_remote, "Q:")
})


test_that("Can fall-back onto default drive if requested", {
  root <- tempfile()
  mounts <- example_mounts(root)
  home <- file.path(root, "home")
  res <- dide_detect_mount_home(home, mounts, "bob")
  expect_equal(res, dide_detect_mount_home(NULL, mounts, "bob"))
})


test_that("Can leave home unmounted", {
  mounts <- example_mounts(tempfile())
  expect_null(dide_detect_mount_home(FALSE, mounts, "bob"))
})


test_that("Fail to auto-detect home if ambiguous or impossible", {
  root <- tempfile()
  mounts <- example_mounts(root)
  expect_error(
    dide_detect_mount_home(NULL, mounts[1, , drop = FALSE], "bob"),
    "I can't find your home directory!  Please mount it")
  expect_error(
    dide_detect_mount_home(NULL, mounts[c(1, 2, 2, 3), , drop = FALSE], "bob"),
    "I am confused about your home directory; there are 2 choices")
  expect_error(
    dide_detect_mount_home(TRUE, mounts, "bob"),
    "Unexpected type for 'home'")
})


test_that("Autodetect temp", {
  root <- tempfile()
  mounts <- example_mounts(root)
  res <- dide_detect_mount_temp(NULL, mounts)
  expect_equal(res$name, "temp")
  expect_true(same_path(res$path_local, file.path(root, "temp")))
  expect_equal(res$path_remote, "\\\\fi--didef3.dide.ic.ac.uk\\tmp")
  expect_equal(res$drive_remote, "T:")
})


test_that("Can fall-back onto default drive if requested", {
  root <- tempfile()
  mounts <- example_mounts(root)
  temp <- file.path(root, "temp")
  res <- dide_detect_mount_temp(temp, mounts)
  expect_equal(res, dide_detect_mount_temp(NULL, mounts))
})


test_that("Fail to auto-detect temp if ambiguous or impossible", {
  mounts <- example_mounts(tempfile())
  expect_null(
    dide_detect_mount_temp(NULL, mounts[1, , drop = FALSE]))
  expect_error(
    dide_detect_mount_temp(NULL, mounts[c(1, 4, 4, 3), , drop = FALSE]),
    "I am confused about your temp directory; there are 2 choices")
  expect_error(
    dide_detect_mount_temp(TRUE, mounts),
    "Unexpected type for 'temp'")
})


test_that("dide_detect_mount", {
  ## This is not too hard but currently exploits the fact that paths
  ## can be mounted anywhere on unix. We will need a windows-specific
  ## set of tests here.
  skip_on_os("windows")
  root <- tempfile()
  dir.create(root, FALSE, TRUE)
  root <- normalizePath(root, mustWork = TRUE)
  mounts <- example_mounts(root)
  expect_message(
    res <- dide_detect_mount(mounts, NULL, NULL, NULL, NULL, NULL, FALSE, NULL),
    "Running out of place: .+ is not on a network share")

  ## If we've already told it we'll be working within a drive, all is ok
  workdir <- file.path(root, "home", "path")
  expect_silent(
    res2 <- dide_detect_mount(mounts, NULL, NULL, NULL, workdir, NULL, FALSE, NULL))
  expect_equal(res2, res)

  ## If we are working within a drive we never explicitly mounted,
  ## let's mount that
  workdir <- file.path(root, "proj", "sub")
  res3 <- dide_detect_mount(mounts, NULL, NULL, NULL, workdir, NULL, FALSE, NULL)
  expect_equal(res3[1:2], res)
  expect_length(res3, 3)
  expect_s3_class(res3$workdir, "path_mapping")
  expect_true(same_path(res3$workdir$path_local, file.path(root, "proj")))
  expect_equal(res3$workdir$path_remote,
               "\\\\fi--didenas1.dide.ic.ac.uk\\Project")

  ## This will only be ambiguous rarely by this point:
  workdir <- file.path(root, "proj", "sub")
  expect_error(
    dide_detect_mount_find_workdir(res, workdir, mounts[c(1:4, 1:4), ]),
    "Having trouble determining the working directory mount point")
})


test_that("Find an available drive", {
  shares <- list(list(drive_remote = "V:"),
                 list(drive_remote = "W:"))
  expect_equal(available_drive(shares, "X:"), "X:")
  expect_equal(available_drive(shares, "/path"), "X:")
  expect_equal(available_drive(list(), "/path"), "V:")
})


test_that("Validate additional shares", {
  mounts <- example_mounts(tempfile())
  shares <- Map(path_mapping,
                c("other", "home", "project", "temp", "sk"),
                mounts[, "local"],
                mounts[, "remote"],
                c("O:", "Q:", "P:", "T:", "K:"))
  expect_silent(dide_detect_mount_check_shares(shares))
  expect_equal(dide_detect_mount_check_shares(shares[[1]]), shares[1])
  expect_error(dide_detect_mount_check_shares(c(shares, TRUE)),
               "All elements of 'shares' must be a path_mapping")
  expect_error(dide_detect_mount_check_shares(TRUE),
               "Invalid input for 'shares'")
})


test_that("Prevent duplicated drives", {
  mounts <- example_mounts(tempfile())
  shares <- Map(path_mapping,
                c("other", "project"),
                mounts[c(1, 3), "local"],
                mounts[c(1, 3), "remote"],
                c("O:", "T:"))
  expect_error(
    dide_detect_mount(mounts, shares, NULL, NULL, NULL, "bob", FALSE, NULL),
    "Duplicate remote drive names: T:")
})

test_that("Remap nas regex - Paddington", {
  expect_equal(use_app_on_nas_paddington("\\\\fi--didenas1.dide.ic.ac.uk\\X"), "\\\\fi--didenas1-app.dide.local\\X")
  expect_equal(use_app_on_nas_paddington("//fi--didenas3.dide.ic.ac.uk/X"), "//fi--didenas3-app.dide.local/X")
  expect_equal(use_app_on_nas_paddington("\\\\fi--didenas4\\X"), "\\\\fi--didenas4-app\\X")
  expect_equal(use_app_on_nas_paddington("//fi--didenas5/X"), "//fi--didenas5-app/X")
  expect_equal(use_app_on_nas_paddington("\\\\fi--didenas1.dide.local\\X"), "\\\\fi--didenas1-app.dide.local\\X")
  expect_equal(use_app_on_nas_paddington("//fi--didenas3.dide.local/X"), "//fi--didenas3-app.dide.local/X")

  expect_equal(use_app_on_nas_paddington("\\\\fi--didenas1-app\\X"), "\\\\fi--didenas1-app\\X")
  expect_equal(use_app_on_nas_paddington("//fi--didenas3-app/X"), "//fi--didenas3-app/X")
  expect_equal(use_app_on_nas_paddington("\\\\fi--didenas4-app.dide.local\\X"), "\\\\fi--didenas4-app.dide.local\\X")
  expect_equal(use_app_on_nas_paddington("//fi--didenas5-app.dide.local/X"), "//fi--didenas5-app.dide.local/X")

})

test_that("Remap nas regex - South Ken", {
  expect_equal(use_app_on_nas_south_ken("\\\\wpia-hn/X"), "\\\\wpia-hn-app/X")
  expect_equal(use_app_on_nas_south_ken("//wpia-hn/X"), "//wpia-hn-app/X")
  expect_equal(use_app_on_nas_south_ken("\\\\wpia-hn.hpc.dide.ic.ac.uk\\X"), "\\\\wpia-hn-app.hpc.dide.local\\X")
  expect_equal(use_app_on_nas_south_ken("//wpia-hn.hpc.dide.ic.ac.uk/X"), "//wpia-hn-app.hpc.dide.local/X")
  expect_equal(use_app_on_nas_south_ken("\\\\wpia-hn.dide.local\\X"), "\\\\wpia-hn-app.hpc.dide.local\\X")
  expect_equal(use_app_on_nas_south_ken("//wpia-hn.dide.local/X"), "//wpia-hn-app.hpc.dide.local/X")
  expect_equal(use_app_on_nas_south_ken("\\\\wpia-hn.hpc.dide.local\\X"), "\\\\wpia-hn-app.hpc.dide.local\\X")
  expect_equal(use_app_on_nas_south_ken("//wpia-hn.hpc.dide.local/X"), "//wpia-hn-app.hpc.dide.local/X")
})

test_that("Check nas regex won't map cross-campus", {
  expect_equal(use_app_on_nas_paddington("\\\\wpia-hn/X"), "\\\\wpia-hn/X")
  expect_equal(use_app_on_nas_paddington("//wpia-hn/X"), "//wpia-hn/X")
  expect_equal(use_app_on_nas_paddington("\\\\wpia-hn.hpc.dide.ic.ac.uk\\X"), "\\\\wpia-hn.hpc.dide.ic.ac.uk\\X")
  expect_equal(use_app_on_nas_paddington("//wpia-hn.hpc.dide.ic.ac.uk/X"), "//wpia-hn.hpc.dide.ic.ac.uk/X")
  expect_equal(use_app_on_nas_paddington("\\\\wpia-hn.dide.local\\X"), "\\\\wpia-hn.dide.local\\X")
  expect_equal(use_app_on_nas_paddington("//wpia-hn.dide.local/X"), "//wpia-hn.dide.local/X")
  expect_equal(use_app_on_nas_paddington("\\\\wpia-hn.hpc.dide.local\\X"), "\\\\wpia-hn.hpc.dide.local\\X")
  expect_equal(use_app_on_nas_paddington("//wpia-hn.hpc.dide.local/X"), "//wpia-hn.hpc.dide.local/X")

  expect_equal(use_app_on_nas_south_ken("\\\\fi--didenas1.dide.ic.ac.uk\\X"), "\\\\fi--didenas1.dide.ic.ac.uk\\X")
  expect_equal(use_app_on_nas_south_ken("//fi--didenas3.dide.ic.ac.uk/X"), "//fi--didenas3.dide.ic.ac.uk/X")
  expect_equal(use_app_on_nas_south_ken("\\\\fi--didenas4\\X"), "\\\\fi--didenas4\\X")
  expect_equal(use_app_on_nas_south_ken("//fi--didenas5/X"), "//fi--didenas5/X")
  expect_equal(use_app_on_nas_south_ken("\\\\fi--didenas1.dide.local\\X"), "\\\\fi--didenas1.dide.local\\X")
  expect_equal(use_app_on_nas_south_ken("//fi--didenas3.dide.local/X"), "//fi--didenas3.dide.local/X")
})

test_that("Remap nas in Paddington", {
  mounts <- example_mounts(tempfile())
  shares <- Map(path_mapping,
                c("other", "project", "sk"),
                mounts[c(1, 3, 5), "local"],
                mounts[c(1, 3, 5), "remote"],
                c("O:", "P:", "K:"))

  res1 <- dide_detect_mount(mounts, shares, NULL, NULL, NULL, "bob", TRUE, "fi--didemrchnb")
  res2 <- dide_detect_mount(mounts, shares, NULL, NULL, NULL, "bob", FALSE, "fi--didemrchnb")
  expect_equal(res1[1:3], res2[1:3])

  # NAS1 should be remapped to -app on fi--didemrchnb if we ask for it.

  expect_equal(res1[[4]]$path_remote,
               "\\\\fi--didenas1-app.dide.local\\Project")
  expect_equal(res2[[4]]$path_remote,
               "\\\\fi--didenas1.dide.ic.ac.uk\\Project")
  expect_equal(res1[[4]][-2], res2[[4]][-2])

  # wpia-hn shouldn't be remapped to -app on fi--didemrchnb

  expect_equal(res1[[5]]$path_remote,
               "\\\\wpia-hn.hpc.dide.ic.ac.uk\\newshare")
  expect_equal(res2[[5]]$path_remote,
               "\\\\wpia-hn.hpc.dide.ic.ac.uk\\newshare")
  expect_equal(res1[[5]][-2], res2[[5]][-2])
})

test_that("Remap nas in South Ken", {
  mounts <- example_mounts(tempfile())
  shares <- Map(path_mapping,
                c("other", "project", "sk"),
                mounts[c(1, 3, 5), "local"],
                mounts[c(1, 3, 5), "remote"],
                c("O:", "P:", "K:"))
  res1 <- dide_detect_mount(mounts, shares, NULL, NULL, NULL, "bob", TRUE, "wpia-hn")
  res2 <- dide_detect_mount(mounts, shares, NULL, NULL, NULL, "bob", FALSE, "wpia-hn")
  expect_equal(res1[1:3], res2[1:3])

  # nas1 shouldn't be "-app" on the wpia-hn cluster in either case

  expect_equal(res1[[4]]$path_remote,
               "\\\\fi--didenas1.dide.ic.ac.uk\\Project")
  expect_equal(res2[[4]]$path_remote,
               "\\\\fi--didenas1.dide.ic.ac.uk\\Project")
  expect_equal(res1[[4]][-2], res2[[4]][-2])

  # wpia-hn should be "-app" on the wpia-hn cluster if we want remapping

  expect_equal(res1[[5]]$path_remote,
               "\\\\wpia-hn-app.hpc.dide.local\\newshare")
  expect_equal(res2[[5]]$path_remote,
               "\\\\wpia-hn.hpc.dide.ic.ac.uk\\newshare")
  expect_equal(res1[[5]][-2], res2[[5]][-2])

})
