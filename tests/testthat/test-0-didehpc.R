context("didehpc")

test_that("home directory", {
  expect_true(dide_home("foo", "bar") %in%
               c("\\\\fi--san03\\homes\\bar\\foo",
                 "\\\\fi--san03.dide.ic.ac.uk\\homes\\bar\\foo"))
  expect_error(dide_home("foo"), "is missing")
})
