context("communication")

test_that("Simple credential handling", {
  withr::with_options(
    list(didehpc.credentials = NULL, didehpc.username = NULL), {
      expect_null(simple_credentials(NULL))
      expect_equal(simple_credentials("bob"), "bob")
      expect_equal(simple_credentials(
        structure(list(credentials = "bob"), class = "didehpc_config")),
        "bob")
    })

  withr::with_options(
    list(didehpc.credentials = "alice", didehpc.username = "bob"), {
      expect_equal(simple_credentials(NULL), "alice")
      expect_equal(simple_credentials("bob"), "bob")
    })

  withr::with_options(
    list(didehpc.credentials = NULL, didehpc.username = "alice"), {
      expect_equal(simple_credentials(NULL), "alice")
      expect_equal(simple_credentials("bob"), "bob")
    })
})


test_that("Simple client", {
  mock_client <- R6::R6Class(
    "mock_client",
    public = list(
      initialize = function(...) {
        self$args <- list(...)
      },
      args = NULL
    ))
  res <- simple_client(list(username = "bob", password = "secret"),
                       mock_client)
  expect_is(res, "mock_client")
  expect_equal(
    res$args,
    list(
      list(username = "bob", password = password("secret")),
      login = TRUE))
})


test_that("cluster load interface", {
  mock_client <- mockery::mock(
    list(load_overall = function() "a"),
    cycle = TRUE)
  mockery::stub(cluster_load, "simple_client", mock_client)

  expect_output(cluster_load(), "a")
  mockery::expect_called(mock_client, 1L)
  expect_equal(mockery::mock_args(mock_client)[[1]], list(NULL))

  expect_output(cluster_load(list(credentials = "bob")), "a")
  mockery::expect_called(mock_client, 2L)
  expect_equal(mockery::mock_args(mock_client)[[2]],
               list(list(credentials = "bob")))
})


test_that("login interface", {
  mock_client <- mockery::mock()
  mockery::stub(web_login, "simple_client", mock_client)

  expect_true(web_login())
  mockery::expect_called(mock_client, 1L)
  expect_equal(mockery::mock_args(mock_client)[[1]], list(NULL))

  expect_true(web_login(list(credentials = "bob")))
  mockery::expect_called(mock_client, 2L)
  expect_equal(mockery::mock_args(mock_client)[[2]],
               list(list(credentials = "bob")))
})
