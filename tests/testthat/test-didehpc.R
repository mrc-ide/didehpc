context("didehpc")

test_that("home directory", {
  expect_equal(dide_home("foo", "bar"),
               "\\\\fi--san02\\homes\\bar\\foo")
  expect_error(dide_home("foo"), "is missing")
})
