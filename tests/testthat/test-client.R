context("client")

test_that("Can create api client", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials, "fi--dideclusthn")
  expect_false(cl$logged_in())
  expect_equal(cl$username(), credentials$username)
})


test_that("login sends sensible data", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials, "fi--dideclusthn")
  mock_login <- mockery::mock(cycle = TRUE)
  mock_post <- mockery::mock(mock_response(200), mock_response(403))
  mockery::stub(cl$login, "api_client_login", mock_login)
  mockery::stub(cl$logged_in, "self$POST", mock_post)

  cl$login(public = TRUE)
  expect_false(cl$logged_in())
  mockery::expect_called(mock_login, 0)
  mockery::expect_called(mock_post, 0)

  cl$login()
  mockery::expect_called(mock_login, 1)
  expect_equal(mockery::mock_args(mock_login)[[1]],
               unname(credentials))

  expect_true(cl$logged_in())
  mockery::expect_called(mock_post, 1)
  expect_equal(mockery::mock_args(mock_post)[[1]],
               list("/_listheadnodes.php", list(user = "")))

  cl$login(refresh = FALSE)
  mockery::expect_called(mock_login, 1)
  mockery::expect_called(mock_post, 1)

  cl$login(refresh = TRUE)
  mockery::expect_called(mock_login, 2)

  expect_false(cl$logged_in())
  mockery::expect_called(mock_post, 2)
})


test_that("request handles http requests", {
  verb <- mockery::mock(mock_response(200),
                        mock_response(403),
                        mock_response(400))
  credentials <- example_credentials()
  cl <- api_client$new(credentials, "fi--dideclusthn")
  data <- list(a = 1, b = 2)
  cl$request(verb, "/path/to", data = data, public = TRUE)
  expect_error(
    cl$request(verb, "/path/to", data = data, public = TRUE),
    "Please login first")
  expect_error(
    cl$request(verb, "/path/to", data = data, public = TRUE),
    "400")

  mockery::expect_called(verb, 3)
  expect_equal(
    mockery::mock_args(verb),
    rep(list(list("https://mrcdata.dide.ic.ac.uk/hpc/path/to",
                  data = data)), 3))
})


test_that("GET forwards args to request", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials, "fi--dideclusthn")
  mock_request <- mockery::mock()
  mockery::stub(cl$GET, "self$request", mock_request)
  cl$GET("/api/v1/cluster_software/", public = TRUE)
  mockery::expect_called(mock_request, 1L)
  expect_equal(
    mockery::mock_args(mock_request)[[1]],
    list(httr::GET, "/api/v1/cluster_software/", public = TRUE))
})


test_that("POST forwards args to request", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials, "fi--dideclusthn")
  mock_request <- mockery::mock()
  mockery::stub(cl$POST, "self$request", mock_request)
  data <- list(a = "a", b = "b")
  cl$POST("/_listheadnodes.php", data, public = TRUE)
  mockery::expect_called(mock_request, 1L)
  ## Many more options here than above:
  expect_equal(
    mockery::mock_args(mock_request)[[1]],
    list(httr::POST, "/_listheadnodes.php", data, public = TRUE,
         httr::accept("text/plain"), encode = "form"))
})


test_that("Can send sensible login request", {
  mock_post <- mockery::mock(
    mock_response(403, content = "Some error"),
    mock_response(200,
                  content = "<p>You don't seem to have any HPC access</p>"),
    mock_response(200,
                  content = "Welcome"))
  mockery::stub(api_client_login, "httr::POST", mock_post)

  expect_error(
    api_client_login("username", "password"), "403")
  expect_error(
    api_client_login("username", "password"),
    "You do not have HPC access - please contact Wes")
  expect_silent(api_client_login("username", "password"))

  mockery::expect_called(mock_post, 3)
  expect_equal(
    mockery::mock_args(mock_post)[[1]],
    list("https://mrcdata.dide.ic.ac.uk/hpc/index.php",
         body = list(us = encode64("username"),
                     pw = encode64("password"),
                     hpcfunc = encode64("login")),
         encode = "form"))
})
