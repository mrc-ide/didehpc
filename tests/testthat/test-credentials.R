context("credentials")

test_that("Prompt for credentials must be run interactively", {
  mock_interactive <- mockery::mock(FALSE)
  mockery::stub(prompt_credentials, "interactive", mock_interactive)
  expect_error(prompt_credentials(),
               "Credentials file needed for non-interactive use")
})


test_that("Require a sensible name", {
  mock_interactive <- mockery::mock(TRUE, cycle = TRUE)
  mock_readline <- mockery::mock("", "bob")
  mockery::stub(prompt_credentials, "interactive", mock_interactive)
  mockery::stub(prompt_credentials, "readline", mock_readline)
  expect_error(prompt_credentials(),
               "Invalid empty username")
  expect_equal(prompt_credentials(), "bob")
})


test_that("get_credentials does reasonable things", {
  expect_equal(get_credentials("bob", FALSE), list(username = "bob"))
  expect_equal(get_credentials(list(username = "bob"), FALSE),
               list(username = "bob"))
  expect_equal(get_credentials(list(username = "bob", password = "secret")),
               list(username = "bob", password = "secret"))
  expect_error(get_credentials(list(username = "bob", pass = "secret")),
               "Unknown fields in credentials: pass")
  expect_error(get_credentials(list(username = "bob")),
               "Missing fields in credentials: password")
  expect_error(get_credentials(list("bob")),
               "Credentials must be named")
  expect_error(get_credentials(1),
               "Unexpected type for credentials")
})


test_that("read smb credentials", {
  path <- tempfile()
  writeLines(c("username=bob", "password=secret"), path)
  expect_equal(read_credentials(path, TRUE),
               list(username = "bob", password = "secret"))
  expect_equal(get_credentials(path),
               list(username = "bob", password = "secret"))
  writeLines(c("user=bob", "pass=secret"), path)
  expect_error(read_credentials(path, TRUE),
               "Unknown fields in credentials")
})


test_that("Prompt for credentials interactively", {
  mock_prompt_credentials <- mockery::mock("bob")
  mockery::stub(get_credentials, "prompt_credentials", mock_prompt_credentials)
  expect_equal(get_credentials(NULL, FALSE),
               list(username = "bob"))
})


test_that("Prompt for password if needed", {
  mock_interactive <- mockery::mock(FALSE, TRUE)
  mock_getpass <- mockery::mock("secret")
  mockery::stub(get_credentials, "interactive", mock_interactive)
  mockery::stub(get_credentials, "getPass::getPass", mock_getpass)
  expect_error(get_credentials("bob"),
               "Credentials file needed for non-interactive use")
  expect_equal(get_credentials("bob"),
               list(username = "bob", password = "secret"))
  mockery::expect_called(mock_getpass, 1)
  expect_equal(mockery::mock_args(mock_getpass)[[1]],
               list("Enter DIDE password for bob: "))
})
