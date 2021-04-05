context("util")

test_that("Can detect platform", {
  mock_sys_info <- mockery::mock(
    list(sysname = "Windows"),
    list(sysname = "Darwin"),
    list(sysname = "Linux"),
    cycle = TRUE)
  mockery::stub(is_windows, "Sys.info", mock_sys_info)
  mockery::stub(is_linux, "Sys.info", mock_sys_info)
  expect_true(is_windows())
  expect_false(is_windows())
  expect_false(is_windows())
  expect_false(is_linux())
  expect_false(is_linux())
  expect_true(is_linux())
})


test_that("base 64 encoding works for zero length strings", {
  expect_equal(encode64(""), "")
  expect_equal(decode64(""), "")
  expect_equal(decode64(encode64("hello")), "hello")
})


test_that("modify_list updates a list", {
  x <- list(a = 1, b = 2, c = 3)
  expect_equal(modify_list(x, list(b = 20)),
               list(a = 1, b = 20, c = 3))
  expect_error(modify_list(x, list(x = 20)),
               "Unknown elements in .+: x")
})


test_that("backup files", {
  tmp <- tempfile()
  dir.create(tmp, FALSE, TRUE)
  path <- file.path(tmp, "file")

  writeLines("1", path)
  expect_message(expect_true(backup(path)), "Copying .+ -> .+\\.1")
  writeLines("2", path)
  expect_silent(expect_true(backup(path, FALSE)))
  writeLines("3", path)

  expect_setequal(dir(tmp), c("file", "file.1", "file.2"))
  expect_equal(readLines(path), "3")
  expect_equal(readLines(paste0(path, ".1")), "1")
  expect_equal(readLines(paste0(path, ".2")), "2")
})


test_that("assertions work", {
  expect_equal(match_value("aaa", c("aaa", "bbb", "ccc")), "aaa")
  expect_error(
    match_value("abc", c("aaa", "bbb", "ccc")),
    "'.+' must be one of 'aaa', 'bbb', 'ccc'")

  expect_silent(assert_scalar(1))
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(letters), "'letters' must be a scalar")

  expect_silent(assert_character("a"))
  expect_error(assert_character(pi), "'pi' must be a character")
})


test_that("throttle", {
  a <- 0
  f <- function(n) {
    a <<- a + n
  }
  g <- throttle(f, 0.01)
  t1 <- Sys.time() + 0.1
  while (Sys.time() < t1) {
    g(1)
  }
  expect_gte(a, 10) # 0.1 / 0.01
})
