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

  expect_silent(assert_scalar_integer(1L))
  expect_silent(assert_scalar_integer(1))
  expect_error(assert_scalar_integer(1.1),
               "must be an integer")
})


test_that("system_intern_check copes with R's weirdnesses", {
  sys <- function(outcome) {
    if (outcome == "success") {
      "result"
    } else if (outcome == "failure1") {
      warning("failure")
      structure("result", status = 1)
    } else if (outcome == "failure2") {
      stop("failure")
    }
  }

  mock_system <- mockery::mock(sys("success"),
                               sys("failure1"),
                               sys("failure2"))
  mockery::stub(system_intern_check, "system", mock_system)
  expect_equal(system_intern_check("some command"), "result")
  expect_error(system_intern_check("some command"), "Error running command")
  expect_error(system_intern_check("some command"), "failure")

  mockery::expect_called(mock_system, 3)
  expect_equal(mockery::mock_args(mock_system),
               rep(list(list("some command", intern = TRUE)), 3))
})


test_that("readlines_if_exists returns NULL for missing file", {
  expect_null(readlines_if_exists(tempfile()))
})


test_that("readlines_if_exists does not warn on incomplete file", {
  txt <- "line1\nline2"
  path <- tempfile()
  writeBin(charToRaw(txt), path)
  expect_silent(
    r <- readlines_if_exists(path, warn = FALSE))
  expect_equal(r, c("line1", "line2"))
})


test_that("sys_which throws on unknown exe", {
  expect_error(sys_which("unknowncommand"),
               "unknowncommand not found in $PATH",
               fixed = TRUE)
})
