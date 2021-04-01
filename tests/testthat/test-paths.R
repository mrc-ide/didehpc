context("paths")

test_that("can create a path mapping", {
  p <- getwd()
  m <- path_mapping("home", p, "//fi--san03/homes/bob", "Q:")
  expect_is(m, "path_mapping")
  str <- as.character(m)
  expect_match(str, "\\(local\\) .+ => .+ \\(remote\\)")
  expect_output(print(m), str, fixed = TRUE)
})

test_that("can validate creation of path mapping", {
  expect_error(
    path_mapping("home", tempfile(), "//fi--san03/homes/bob", "Q:"),
    "Local mount point does not exist: ")
  expect_error(
    path_mapping("home", "Q:", "Q://fi--san03/homes/bob", "Q:"),
    "Local mount point does not exist: Q:/")
  expect_error(
    path_mapping("home", getwd(), "//fi--san03/homes/bob", "Q"),
    "drive_remote must be of the form 'X:'")
})


test_that("Can clean a remote path", {
  expect_equal(
    clean_path_remote("//fi--san03/homes/bob"),
    "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob")
  expect_equal(
    clean_path_remote("//fi--san03.dide.local/homes/bob"),
    "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob")
  expect_equal(
    clean_path_remote("//fi--san03.dide.ic.ac.uk/homes/bob/"),
    "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob")
})


test_that("Can detect a path into a share", {
  p <- dirname(getwd())
  t <- tempfile()
  dir.create(t)
  shares <- list(
    home = path_mapping("home", p, "//fi--san03/homes/bob", "Q:"),
    temp = path_mapping("temp", tempdir(), "//fi--san03/tmp", "T:"))

  x <- prepare_path(t, shares)
  expect_equal(x$rel, basename(t))
  expect_s3_class(x, "path_mapping")
  expect_equal(x[names(x) != "rel"], shares$temp[])
  str <- as.character(x)
  expect_match(str, "\\[rel: .+\\] \\(local\\) .+ => .+ => T: \\(remote\\)")
})


test_that("prepare_path rejects nonexistant paths", {
  expect_error(
    prepare_path(tempfile(tmpdir = getwd()), list()),
    "path does not exist:")
})


test_that("prepare_path handles unmapped paths", {
  expect_error(
    prepare_path(getwd(), list()),
    "did not find network mapping for path")
  expect_null(
    prepare_path(getwd(), list(), FALSE))
})


test_that("Can create a remote path", {
  p <- dirname(getwd())
  t <- tempfile()
  dir.create(t)
  shares <- list(
    home = path_mapping("home", p, "//fi--san03/homes/bob", "Q:"),
    temp = path_mapping("temp", tempdir(), "//fi--san03/tmp", "T:"))
  res <- remote_path(t, shares)
  expect_equal(
    res,
    paste0("\\\\fi--san03.dide.ic.ac.uk\\tmp\\", basename(t)))
})


test_that("file_path elides NULL elements", {
  expect_equal(file_path("a", "b"), file.path("a", "b"))
  expect_equal(file_path("a", NULL, "b"), file.path("a", "b"))
})


test_that("dide path helpers return windows network paths", {
  expect_equal(
    dide_home("a/b", "bob"),
    "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob\\a\\b")
  expect_equal(
    dide_home(c("a\\b", "x"), "bob"),
    c("\\\\fi--san03.dide.ic.ac.uk\\homes\\bob\\a\\b",
      "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob\\x"))
  expect_equal(
    dide_temp("x/y"),
    "\\\\fi--didef3.dide.ic.ac.uk\\tmp\\x\\y")
  expect_equal(
    dide_temp(c("x\\y", "a")),
    c("\\\\fi--didef3.dide.ic.ac.uk\\tmp\\x\\y",
      "\\\\fi--didef3.dide.ic.ac.uk\\tmp\\a"))
})


test_that("Construct internals paths", {
  root <- tempdir()
  expect_equal(path_batch(root), file.path(root, "batch"))
  expect_equal(path_batch(root, "id"), file.path(root, "batch", "id.bat"))

  expect_equal(path_logs(root), file.path(root, "logs"))
  expect_equal(path_logs(root, "id"), file.path(root, "logs", "id"))

  expect_equal(path_worker_logs(root), file.path(root, "workers"))
  expect_equal(path_worker_logs(root, "id"), file.path(root, "workers", "id"))
})
