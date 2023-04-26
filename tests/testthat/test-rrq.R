context("rrq")

test_that("queue interface to worker submission sends correct args", {
  config <- example_config(use_workers = TRUE)
  ctx <- context::context_save(file.path(config$workdir, "context"))
  test_fake_rrq(ctx, config)
  obj <- queue_didehpc(ctx, config, initialise = FALSE, provision = "fake")
  mock_submit_workers <- mockery::mock()
  mockery::stub(obj$submit_workers, "rrq_submit_workers", mock_submit_workers)
  obj$submit_workers(5, 100, FALSE)
  mockery::expect_called(mock_submit_workers, 1)
  expect_equal(mockery::mock_args(mock_submit_workers)[[1]],
               list(obj, r6_private(obj)$data, 5, 100, FALSE))
})


test_that("queue interface to job submission sends correct args", {
  config <- example_config(use_workers = TRUE)
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc(ctx, config, initialise = FALSE, provision = "fake")
  mock_submit_context_tasks <- mockery::mock()
  ids <- ids::random_id(5)
  mockery::stub(obj$submit, "rrq_submit_context_tasks",
                mock_submit_context_tasks)
  obj$submit(ids)
  mockery::expect_called(mock_submit_context_tasks, 1)
  expect_equal(mockery::mock_args(mock_submit_context_tasks)[[1]],
               list(obj$config, obj$context, ids, NULL))
})


test_that("queue interface to worker stop sends correct args", {
  config <- example_config(use_workers = TRUE)
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc(ctx, config, initialise = FALSE, provision = "fake")
  mock_stop_workers <- mockery::mock()
  mockery::stub(obj$stop_workers, "rrq_stop_workers", mock_stop_workers)
  ids <- ids::random_id(2)
  obj$stop_workers(ids)
  mockery::expect_called(mock_stop_workers, 1)
  expect_equal(mockery::mock_args(mock_stop_workers)[[1]],
               list(obj$config, obj$context$id, ids))
})


test_that("queue interface to controller creation sends correct args", {
  config <- example_config(use_workers = TRUE)
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc(ctx, config, initialise = FALSE, provision = "fake")
  mock_rrq_controller <- mockery::mock()
  mockery::stub(obj$rrq_controller, "didehpc_rrq_controller",
                mock_rrq_controller)
  obj$rrq_controller()
  mockery::expect_called(mock_rrq_controller, 1)
  expect_equal(mockery::mock_args(mock_rrq_controller)[[1]],
               list(obj$config, obj$context$id))
})


test_that("stopping workers interaction test", {
  config <- example_config(use_workers = TRUE)
  id <- ids::random_id()
  worker_ids <- ids::random_id(4)
  mock_rrq <- list(worker_stop = mockery::mock())
  mock_rrq_controller <- mockery::mock(mock_rrq)
  mockery::stub(rrq_stop_workers, "didehpc_rrq_controller", mock_rrq_controller)
  rrq_stop_workers(config, id, worker_ids)

  mockery::expect_called(mock_rrq_controller, 1)
  expect_equal(mockery::mock_args(mock_rrq_controller)[[1]],
               list(config, id))

  mockery::expect_called(mock_rrq$worker_stop, 1)
  expect_equal(mockery::mock_args(mock_rrq$worker_stop)[[1]],
               list(worker_ids))
})


## this tests makes the manipulation below more reasonable because it
## makes clear which parts of config are used.
test_that("rrq controller reads the cluster host", {
  mock_hiredis <- mockery::mock(redux::redis)
  mock_rrq <- mockery::mock()
  id <- ids::random_id()
  mockery::stub(didehpc_rrq_controller, "rrq::rrq_controller$new", mock_rrq)
  mockery::stub(didehpc_rrq_controller, "redux::hiredis", mock_hiredis)
  expect_error(
    didehpc_rrq_controller(list(use_rrq = FALSE, use_workers = FALSE), id),
    "workers not enabled")
  mockery::expect_called(mock_hiredis, 0)
  mockery::expect_called(mock_rrq, 0)

  config <- list(use_rrq = TRUE,
                 use_workers = FALSE,
                 redis_host = "redis.example.com")
  expect_silent(didehpc_rrq_controller(config, id))
  mockery::expect_called(mock_hiredis, 1)
  mockery::expect_called(mock_rrq, 1)
  expect_equal(mockery::mock_args(mock_hiredis)[[1]],
               list(host = "redis.example.com"))
  expect_equal(mockery::mock_args(mock_rrq)[[1]], list(id, redux::redis))
})


test_that("configure environment", {
  skip_if_no_redis()
  config <- list(use_rrq = TRUE, redis_host = NULL, worker_timeout = 100)
  id <- ids::random_id()
  rrq <- didehpc_rrq_controller(config, id)
  expect_null(rrq$con$GET(r6_private(rrq)$keys$envir))

  rrq_init(rrq, config)
  expect_setequal(rrq$worker_config_list(), c("localhost", "didehpc"))
  expect_equal(
    rrq$worker_config_read("didehpc"),
    list(timeout_idle = 100, queue = c("default", "context"), verbose = TRUE,
         timeout_poll = 1, timeout_die = 2))

  create <- rrq$con$GET(r6_private(rrq)$keys$envir)
  expect_is(create, "raw")
  expect_is(unserialize(create), "function")
  expect_identical(environment(unserialize(create)), globalenv())
})


test_that("use context loader", {
  skip_if_no_redis()
  loader <- rrq_context_loader()
  expect_identical(environment(loader), globalenv())
  ctx <- context::context_save(tempfile())
  env <- new.env()

  withr::with_envvar(
    c(CONTEXT_ROOT = NA_character_, CONTEXT_ID = NA_character_),
    expect_error(
      loader(env),
      "Environment variables incorrect: CONTEXT_ROOT: '' CONTEXT_ID: ''"))
  withr::with_envvar(
    c(CONTEXT_ROOT = NA_character_, CONTEXT_ID = "x"),
    expect_error(
      loader(env),
      "Environment variables incorrect: CONTEXT_ROOT: '' CONTEXT_ID: 'x'"))

  mock_load <- mockery::mock()
  mockery::stub(loader, "context::context_load", mock_load)
  withr::with_envvar(
    c(CONTEXT_ROOT = ctx$root$path, CONTEXT_ID = ctx$id),
    loader(env))
  mockery::expect_called(mock_load, 1L)
  args <- mockery::mock_args(mock_load)[[1]]
  expect_equal(args[[1]]$root$id, ctx$root$id)
  expect_equal(args[[1]]$root$path, ctx$root$path)
  expect_equal(args[[1]]$id, ctx$id)
  expect_identical(args[[2]], env)
})


test_that("Can send context tasks to a worker", {
  skip_if_no_redis()
  config <- list(use_rrq = TRUE, redis_host = NULL, worker_timeout = 10)
  ctx <- context::context_save(tempfile())
  ids <- c(context::task_save(quote(sin(1)), ctx),
           context::task_save(quote(sin(2)), ctx))
  rrq_submit_context_tasks(config, ctx, ids, NULL)
  rrq <- didehpc_rrq_controller(config, ctx$id)
  rrq_init(rrq, config)
  rrq$envir(NULL, FALSE) # just simpler not to have this here
  id_rrq <- rrq$task_list()
  expect_length(id_rrq, 2L)

  rrq$task_data(id_rrq[[1]])

  ## TODO: will be exposed; in rrq this is rrq_worker_blocking; mrc-2297
  w <- rrq::rrq_worker_from_config(ctx$id, "didehpc")
  w$step(TRUE)

  expect_equal(context::task_status(ids[[1]], ctx), "COMPLETE")
  expect_equal(context::task_status(ids[[2]], ctx), "PENDING")

  w$step(TRUE)

  expect_equal(context::task_status(ids[[1]], ctx), "COMPLETE")
  expect_equal(context::task_status(ids[[2]], ctx), "COMPLETE")
})


test_that("Can plausibly submit workers", {
  skip_if_no_redis()
  config <- example_config(use_workers = TRUE)
  config$redis_host <- NULL
  ctx <- context::context_save(file.path(config$workdir, "context"))
  test_fake_rrq(ctx, config)
  obj <- queue_didehpc(ctx, config, initialise = FALSE, provision = "fake")
  obj$client <- list(submit = mockery::mock())
  mock_wait <- mockery::mock()
  mockery::stub(rrq_submit_workers, "rrq::rrq_worker_wait", mock_wait)
  data <- r6_private(obj)$data

  expect_message(
    rrq_submit_workers(obj, data, 5, 100, FALSE),
    "Submitting 5 workers with base name '.+'")

  expect_true(file.exists(data$paths$local$batch))
  expect_true(file.exists(data$paths$local$worker_log))
  mockery::expect_called(obj$client$submit, 5)

  batch <- dir(data$paths$local$batch)
  expect_length(batch, 5)
  base <- sub("_[0-9]\\.bat", "", batch[[1]])

  args <- mockery::mock_args(obj$client$submit)
  expect_equal(
    args[[1]],
    list(paste0(data$paths$remote$batch, "\\", base, "_1.bat"),
         paste0(base, "_1"), config$resource$template, config$cluster,
         config$resource$type, config$resource$count))
  expect_equal(
    args[[5]],
    list(paste0(data$paths$remote$batch, "\\", base, "_5.bat"),
         paste0(base, "_5"), config$resource$template, config$cluster,
         config$resource$type, config$resource$count))

  mockery::expect_called(mock_wait, 1)
  args <- mockery::mock_args(mock_wait)[[1]]
  expect_s3_class(args[[1]], "rrq_controller")
  expect_match(args[[2]], sprintf("^%s:worker:alive:.+", ctx$id))
  expect_equal(args[3:4], list(timeout = 100, progress = FALSE))
})


test_that("Can plausibly submit workers with different configuration", {
  skip_if_no_redis()
  w <- worker_resource(template = "8Core", cores = 8, parallel = FALSE)
  config <- example_config(use_rrq = TRUE, worker_resource = w)
  config$redis_host <- NULL
  ctx <- context::context_save(file.path(config$workdir, "context"))
  test_fake_rrq(ctx, config)
  obj <- queue_didehpc(ctx, config, initialise = FALSE, provision = "fake")
  obj$client <- list(submit = mockery::mock())
  mock_wait <- mockery::mock()
  mockery::stub(rrq_submit_workers, "rrq::rrq_worker_wait", mock_wait)
  data <- r6_private(obj)$data

  expect_message(
    rrq_submit_workers(obj, data, 5, 100, FALSE),
    "Submitting 5 workers with base name '.+'")

  expect_true(file.exists(data$paths$local$batch))
  expect_true(file.exists(data$paths$local$worker_log))
  mockery::expect_called(obj$client$submit, 5)

  batch <- dir(data$paths$local$batch)
  expect_length(batch, 5)
  base <- sub("_[0-9]\\.bat", "", batch[[1]])

  args <- mockery::mock_args(obj$client$submit)
  expect_equal(
    args[[1]],
    list(paste0(data$paths$remote$batch, "\\", base, "_1.bat"),
         paste0(base, "_1"), config$worker_resource$template, config$cluster,
         config$worker_resource$type, config$worker_resource$count))
  expect_equal(
    args[[5]],
    list(paste0(data$paths$remote$batch, "\\", base, "_5.bat"),
         paste0(base, "_5"), config$worker_resource$template, config$cluster,
         config$worker_resource$type, config$worker_resource$count))

  mockery::expect_called(mock_wait, 1)
  args <- mockery::mock_args(mock_wait)[[1]]
  expect_s3_class(args[[1]], "rrq_controller")
  expect_match(args[[2]], sprintf("^%s:worker:alive:.+", ctx$id))
  expect_equal(args[3:4], list(timeout = 100, progress = FALSE))
})


test_that("error if remote rrq is too old", {
  curr <- packageVersion("rrq")
  expect_error(
    rrq_check_package_version(curr, numeric_version("0.4.4")),
    "Your remote version of rrq (0.4.4) is too old; must be at least 0.6.21",
    fixed = TRUE)
})


test_that("warn if rrq versions differ", {
  curr <- packageVersion("rrq")
  other <- numeric_version("99.99.99")
  expect_warning(
    rrq_check_package_version(curr, other),
    "rrq versions differ between local (0.6.21) and remote (99.99.99)",
    fixed = TRUE)
})


test_that("silent of rrq versions agree", {
  curr <- packageVersion("rrq")
  expect_silent(
    rrq_check_package_version(curr, curr))
})
