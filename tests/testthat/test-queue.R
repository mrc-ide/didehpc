context("queue")

test_that("Can create queue", {
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc(ctx, config, initialise = FALSE)
  expect_s3_class(obj, "queue_didehpc")
})


test_that("login calls login on the client", {
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  client <- list(
    login = mockery::mock())
  obj <- .R6_queue_didehpc$new(ctx, config, NULL, FALSE, client)

  obj$login()
  mockery::expect_called(client$login, 1)
  expect_equal(mockery::mock_args(client$login)[[1]], list(TRUE))

  obj$login(FALSE)
  mockery::expect_called(client$login, 2)
  expect_equal(mockery::mock_args(client$login)[[2]], list(FALSE))
})


test_that("cluster_load passes through to the client", {
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  client <- list(
    load_show = mockery::mock())
  obj <- .R6_queue_didehpc$new(ctx, config, NULL, FALSE, client)

  obj$cluster_load()
  mockery::expect_called(client$load_show, 1)
  expect_equal(mockery::mock_args(client$load_show)[[1]],
               list(NULL, TRUE))

  obj$cluster_load("fi--didemrchnb", FALSE)
  mockery::expect_called(client$load_show, 2)
  expect_equal(mockery::mock_args(client$load_show)[[2]],
               list("fi--didemrchnb", FALSE))
})


test_that("submission requires provisioning", {
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc(ctx, config, initialise = FALSE)

  ids <- ids::random_id(2)
  mock_submit <- mockery::mock()
  mockery::stub(obj$submit, "submit_dide", mock_submit)

  expect_error(
    obj$submit(ids),
    "Queue is not provisioned; run '$provision_library()'",
    fixed = TRUE)
  mockery::expect_called(mock_submit, 0)

  private <- r6_private(obj)
  private$provisioned <- TRUE

  expect_silent(obj$submit(ids))
  mockery::expect_called(mock_submit, 1)
  expect_equal(mockery::mock_args(mock_submit)[[1]],
               list(obj, private$data, ids, NULL))
})


test_that("Submit job and update db", {
  dide_id <- "462460"
  dide_log <- "The\nlog"
  client <- list(
    submit = mockery::mock(dide_id),
    log = mockery::mock(dide_log, cycle = TRUE),
    cancel = mockery::mock(setNames("OK", dide_id)))

  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- .R6_queue_didehpc$new(ctx, config, NULL, FALSE, client)

  private <- r6_private(obj)
  private$provisioned <- TRUE

  t <- obj$enqueue(sin(1))
  expect_s3_class(t, "queuer_task")
  expect_true(t$id %in% obj$task_list())

  path_batch_win <- paste0(private$data$paths$remote$batch, "\\", t$id, ".bat")

  mockery::expect_called(client$submit, 1)
  expect_equal(
    mockery::mock_args(client$submit)[[1]],
    list(path_batch_win, t$id, "GeneralNodes", config$cluster, "Cores", 1))

  ## These are the database changes made:
  expect_equal(obj$db$get(t$id, "dide_id"), dide_id)
  expect_equal(obj$db$get(t$id, "dide_cluster"), config$cluster)
  expect_equal(obj$db$get(t$id, "log_path"), "logs")

  ## The higher-level interface
  expect_equal(obj$dide_id(t), dide_id)
  expect_equal(obj$dide_id(t$id), dide_id)

  ## Can extract logs
  expect_equal(obj$dide_log(t), dide_log)
  expect_equal(obj$dide_log(t$id), dide_log)
  mockery::expect_called(client$log, 2)
  expect_equal(mockery::mock_args(client$log),
               rep(list(list(dide_id, config$cluster)), 2))

  ## And finally unsubmit
  expect_equal(obj$unsubmit(t$id), "OK")
  mockery::expect_called(client$cancel, 1)
  expect_equal(mockery::mock_args(client$cancel)[[1]],
               list(dide_id, config$cluster))
  expect_equal(t$status(), "CANCELLED")
  expect_error(t$result(), "task [[:xdigit:]]+ is unfetchable: CANCELLED")
})


test_that("Can submit a group", {
  client <- list(
    submit = mockery::mock("1", "2", "3", "4"))
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- .R6_queue_didehpc$new(ctx, config, NULL, FALSE, client)
  private <- r6_private(obj)
  private$provisioned <- TRUE
  grp <- obj$lapply(1:4, sin)
})
