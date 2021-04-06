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


test_that("logout uses correct endpoint", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials, "fi--dideclusthn")
  private <- r6_private(cl)
  private$has_logged_in <- TRUE

  mock_logout <- mockery::mock(cycle = TRUE)
  mock_get <- mockery::mock(mock_response(200))
  mockery::stub(cl$logout, "self$GET", mock_get)

  cl$logout()
  expect_false(private$has_logged_in)
  mockery::expect_called(mock_get, 1)
  expect_equal(
    mockery::mock_args(mock_get)[[1]],
    list("/logout.php", public = TRUE))
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
    list(httr::POST, "/_listheadnodes.php", body = data, public = TRUE,
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


test_that("Create client", {
  credentials <- example_credentials()
  cl <- web_client$new(credentials, login = FALSE)
  expect_s3_class(cl, "web_client")
  expect_false(cl$logged_in())
  expect_s3_class(cl$api_client(), "api_client")
})


test_that("login uses client to login and logout", {
  mock_client <- list(
    login = mockery::mock(),
    logout = mockery::mock())

  cl <- web_client$new(login = FALSE, client = mock_client)
  mockery::expect_called(mock_client$login, 0)
  cl$login()
  mockery::expect_called(mock_client$login, 1)
  expect_equal(mockery::mock_args(mock_client$login),
               list(list(refresh = TRUE)))

  cl <- web_client$new(login = TRUE, client = mock_client)
  mockery::expect_called(mock_client$login, 2)
  expect_equal(mockery::mock_args(mock_client$login),
               rep(list(list(refresh = TRUE)), 2))

  cl$logout()
  mockery::expect_called(mock_client$logout, 1)
  expect_equal(mockery::mock_args(mock_client$logout)[[1]], list())
})


test_that("client checks access", {
  mock_client <- list(
    login = function() NULL)
  mock_headnodes <- mockery::mock(
    character(0),
    "fi--dideclusthn",
    c("fi--dideclusthn", "fi--didemrchnb"),
    cycle = TRUE)
  cl <- web_client$new(cluster_default = "fi--didemrchnb",
                       client = mock_client)
  mockery::stub(cl$check_access, "self$headnodes", mock_headnodes)

  expect_error(
    cl$check_access(),
    "You do not have access to any cluster")
  expect_error(
    cl$check_access(),
    "You do not have access to 'fi--didemrchnb'; try 'fi--dideclusthn'")
  expect_silent(cl$check_access())

  expect_error(
    cl$check_access("fi--dideclusthn"),
    "You do not have access to any cluster")
  expect_silent(cl$check_access("fi--dideclusthn"))
  expect_silent(cl$check_access("fi--dideclusthn"))
})


test_that("submit sends correct payload", {
  dide_id <- "12345"
  content <- sprintf("Job has been submitted. ID: %s.\n", dide_id)
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r, cycle = TRUE))
  cl <- web_client$new(login = FALSE, client = mock_client)
  path <- "\\\\host\\path"

  expect_equal(cl$submit(path, "name", "template"), dide_id)
  mockery::expect_called(mock_client$POST, 1L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/submit_1.php",
         client_body_submit(path, "name", "template", "fi--dideclusthn",
                            "Cores", 1)))

  expect_equal(cl$submit(path, "name", "template",
                         "fi--didemrchnb", "Nodes", 2), dide_id)
  mockery::expect_called(mock_client$POST, 2L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[2]],
    list("/submit_1.php",
         client_body_submit(path, "name", "template", "fi--didemrchnb",
                            "Nodes", 2)))
})


test_that("cancel sends correct payload", {
  dide_id <- "12345"
  content <- sprintf("%s\tOK\n", dide_id)
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r, cycle = TRUE))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_equal(cl$cancel(dide_id), setNames("OK", dide_id))

  mockery::expect_called(mock_client$POST, 1L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/cancel.php",
         client_body_cancel(dide_id, "fi--dideclusthn")))
})


test_that("status sends correct payload", {
  content <- read_lines("responses/status.txt")
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r),
                      username = mockery::mock("bob"))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_equal(cl$status_user(), client_parse_status(content))

  mockery::expect_called(mock_client$username, 1L)
  expect_equal(mockery::mock_args(mock_client$username)[[1]], list())

  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/_listalljobs.php",
         client_body_status("*", "bob", "fi--dideclusthn")))
})


test_that("log sends correct payload", {
  dide_id <- "12345"
  content <- read_lines("responses/log.txt")
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_equal(cl$log(dide_id), client_parse_log(content))

  mockery::expect_called(mock_client$POST, 1L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/showjobfail.php",
         client_body_log(dide_id, "fi--dideclusthn")))
})


test_that("status job sends correct payload", {
  dide_id <- "12345"
  r <- mock_response(200, content = "Running")
  mock_client <- list(GET = mockery::mock(r))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_equal(cl$status_job(dide_id, "fi--didemrchnb"),
               "RUNNING")

  mockery::expect_called(mock_client$GET, 1L)
  expect_equal(
    mockery::mock_args(mock_client$GET)[[1]],
    list("/api/v1/get_job_status/",
         query = list(scheduler = "fi--didemrchnb",
                      jobid = dide_id)))
})


test_that("headnodes sends correct payload", {
  content <- paste0(valid_clusters(), "\n", collapse = "")
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r, cycle = TRUE))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_null(r6_private(cl)$headnodes_)

  expect_equal(cl$headnodes(), valid_clusters())
  expect_equal(r6_private(cl)$headnodes_, valid_clusters())
  mockery::expect_called(mock_client$POST, 1L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/_listheadnodes.php", list(user = "")))

  expect_equal(cl$headnodes(), valid_clusters())
  mockery::expect_called(mock_client$POST, 1L)

  expect_equal(cl$headnodes(TRUE), valid_clusters())
  mockery::expect_called(mock_client$POST, 2L)
  expect_equal(mockery::mock_args(mock_client$POST)[[1]],
               mockery::mock_args(mock_client$POST)[[2]])
})


test_that("load endpoints are correct", {
  content <- read_lines("responses/load.txt")
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r, cycle = TRUE))

  cl <- web_client$new(login = FALSE, client = mock_client)
  private <- r6_private(cl)
  private$headnodes_ <- c("fi--dideclusthn", "fi--didemrchnb")

  cmp1 <- client_parse_load_cluster(content, "fi--dideclusthn")
  cmp2 <- client_parse_load_overall(
    lapply(private$headnodes_, client_parse_load_cluster, txt = content))
  expect_equal(cl$load_node(), cmp1)
  expect_equal(cl$load_overall(), cmp2)

  expect_output(
    expect_equal(
      withVisible(cl$load_show()), list(value = cmp1, visible = FALSE)),
    "wpia-dideclus35")
  expect_output(
    expect_equal(
      withVisible(cl$load_show(TRUE)), list(value = cmp2, visible = FALSE)),
    "didehpc")
})
