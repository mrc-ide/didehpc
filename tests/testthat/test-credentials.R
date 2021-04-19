context("credentials")

test_that("Prompt for credentials must be run interactively", {
  mock_interactive <- mockery::mock(FALSE)
  mockery::stub(prompt_username, "interactive", mock_interactive)
  expect_error(prompt_username(),
               "Credentials file needed for non-interactive use")
})


test_that("Prompt for password must be run interactively", {
  mock_interactive <- mockery::mock(FALSE)
  mockery::stub(prompt_password, "interactive", mock_interactive)
  expect_error(prompt_password("user"),
               "Credentials file needed for non-interactive use")
})


test_that("prompt_username calls readline if interactive", {
  mock_interactive <- mockery::mock(TRUE)
  mock_readline <- mockery::mock("bob")
  mockery::stub(prompt_username, "interactive", mock_interactive)
  mockery::stub(prompt_username, "readline", mock_readline)
  expect_equal(prompt_username(), "bob")
})


test_that("prompt_passowrd calls getPass if", {
  mock_interactive <- mockery::mock(TRUE)
  mock_getpass <- mockery::mock("secret")
  mockery::stub(prompt_password, "interactive", mock_interactive)
  mockery::stub(prompt_password, "getPass::getPass", mock_getpass)
  expect_equal(prompt_password("bob"), "secret")
  mockery::expect_called(mock_getpass, 1)
  expect_equal(mockery::mock_args(mock_getpass)[[1]],
               list("Enter DIDE password for bob: "))
})


test_that("Require a sensible name", {
  expect_error(dide_username(""), "Invalid empty username")
  expect_equal(dide_username("bob"), "bob")
  expect_equal(dide_username("DIDE\\bob"), "bob")
})


test_that("Prompt for a username if required", {
  mock_prompt_username <- mockery::mock("bob")
  mockery::stub(dide_username, "prompt_username", mock_prompt_username)
  expect_equal(dide_username(NULL), "bob")
  mockery::expect_called(mock_prompt_username, 1L)
  expect_equal(mockery::mock_args(mock_prompt_username), list(list()))
})


test_that("Prompt for a password if required", {
  mock_prompt_password <- mockery::mock("secret")
  mockery::stub(dide_credentials, "prompt_password", mock_prompt_password)
  expect_equal(dide_credentials("bob", FALSE),
               list(username = "bob"))
  mockery::expect_called(mock_prompt_password, 0L)
  expect_equal(dide_credentials("bob", TRUE),
               list(username = "bob", password = password("secret")))
  mockery::expect_called(mock_prompt_password, 1L)
  expect_equal(mockery::mock_args(mock_prompt_password), list(list("bob")))
})


test_that("dide_credentials does reasonable things", {
  expect_equal(
    dide_credentials("bob", FALSE),
    list(username = "bob"))
  expect_equal(
    dide_credentials(list(username = "bob"), FALSE),
    list(username = "bob"))
  expect_equal(
    dide_credentials(list(username = "bob", password = "secret"), TRUE),
    list(username = "bob", password = password("secret")))
  expect_error(
    dide_credentials(list(username = "bob", pass = "secret"), TRUE),
    "Unknown fields in credentials: pass")
  expect_error(
    dide_credentials(list("bob"), TRUE),
    "Credentials must be named")
  expect_error(
    dide_credentials(list("bob", password = "secret"), TRUE),
    "Credentials must be named")
  expect_error(
    dide_credentials(1, TRUE),
    "Unexpected type for credentials")
  expect_error(
    dide_credentials(setNames(list(), character(0))),
    "Missing fields in credentials: username")
})


test_that("read smb credentials", {
  path <- tempfile()
  writeLines(c("username=bob", "password=secret"), path)
  expect_equal(read_credentials(path),
               list(username = "bob", password = "secret"))
  expect_equal(dide_credentials(path, TRUE),
               list(username = "bob", password = password("secret")))
  writeLines(c("user=bob", "pass=secret"), path)
  expect_error(dide_credentials(path, TRUE),
               "Unknown fields in credentials")
})
