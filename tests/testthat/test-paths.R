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
