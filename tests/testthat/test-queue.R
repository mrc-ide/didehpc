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
  obj <- queue_didehpc_$new(ctx, config, NULL, FALSE, FALSE, FALSE, client)

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
  obj <- queue_didehpc_$new(ctx, config, NULL, FALSE, FALSE, FALSE, client)

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
               list(obj, private$data, ids, NULL, NULL))
})


test_that("can tell the object to skip provisioning", {
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc(ctx, config, initialise = FALSE)
  expect_false(r6_private(obj)$provisioned)
  obj <- queue_didehpc(ctx, config, initialise = FALSE, provision = "later")
  expect_false(r6_private(obj)$provisioned)
  obj <- queue_didehpc(ctx, config, initialise = FALSE, provision = "fake")
  expect_true(r6_private(obj)$provisioned)
})


test_that("Submit job and update db", {
  dide_id <- "462460"
  dide_log <- "The\nlog"
  client <- list(
    submit = mockery::mock(dide_id),
    log = mockery::mock(dide_log, cycle = TRUE),
    cancel = mockery::mock(setNames("OK", dide_id),
                           setNames("WRONG_STATE", dide_id)))

  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc_$new(ctx, config, NULL, FALSE, FALSE, FALSE, client)

  private <- r6_private(obj)
  private$provisioned <- TRUE

  t <- obj$enqueue(sin(1))
  expect_s3_class(t, "queuer_task")
  expect_true(t$id %in% obj$task_list())

  path_batch_win <- paste0(private$data$paths$remote$batch, "\\", t$id, ".bat")

  mockery::expect_called(client$submit, 1)
  expect_equal(
    mockery::mock_args(client$submit)[[1]],
    list(path_batch_win, t$id, "GeneralNodes", config$cluster, "Cores", 1, ""))

  ## These are the database changes made:
  expect_equal(obj$context$db$get(t$id, "dide_id"), dide_id)
  expect_equal(obj$context$db$get(t$id, "dide_cluster"), config$cluster)
  expect_equal(obj$context$db$get(t$id, "log_path"), "logs")

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

  # Task is already unsubmitted - and this time send task object instead of id

  expect_equal(obj$unsubmit(t), "NOT_RUNNING")

})


test_that("Submit job with dependencies", {
  dide_id <- "462460"
  dide_log <- "The\nlog"
  client <- list(
    submit = mockery::mock(dide_id, cycle = TRUE),
    log = mockery::mock(dide_log, cycle = TRUE)
  )

  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc_$new(ctx, config, NULL, FALSE, FALSE, FALSE, client)

  private <- r6_private(obj)
  private$provisioned <- TRUE

  t <- obj$enqueue(sin(1))
  t2 <- obj$enqueue(sin(1), depends_on = t$id)
  bundle <- obj$enqueue_bulk(1:3, quote(I), depends_on = rep(t$id, 3))
})

test_that("can retry single task", {

  client <- list(
    submit = mockery::mock("1", "2")
  )

  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc_$new(ctx, config, NULL, FALSE, FALSE, FALSE, client)

  private <- r6_private(obj)
  private$provisioned <- TRUE

  t <- obj$enqueue(stop("err"))
  expect_s3_class(t, "queuer_task")
  expect_true(t$id %in% obj$task_list())
  mockery::expect_called(client$submit, 1)

  # run and fail
  ctx <- context::context_load(ctx)
  context::task_run(t$id, ctx)
  expect_equal(t$status(), "ERROR")

  ## And finally retry
  obj$task_retry_failed(t$id)
  mockery::expect_called(client$submit, 2)
  expect_equal(t$status(), "PENDING")
  expect_equal(obj$dide_id(t$id), "2")
})

test_that("can retry bundle", {
  client <- list(
    submit = mockery::mock("1", "2", "3", "4")
  )
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc_$new(ctx, config, NULL, FALSE, FALSE, FALSE, client)
  private <- r6_private(obj)
  private$provisioned <- TRUE
  grp <- obj$lapply(1:2, function(x) stop("err"))

  mockery::expect_called(client$submit, 2)
  expect_equal(obj$dide_id(grp), c("1", "2"))

  # run and fail
  ctx <- context::context_load(ctx)
  lapply(grp$ids, function(x) context::task_run(x, ctx))

  # retry
  obj$task_bundle_retry_failed(grp$name)

  mockery::expect_called(client$submit, 4)
  expect_equal(obj$dide_id(grp), c("3", "4"))
})

test_that("Test support for multiple cancels", {
  client <- list(submit = mockery::mock("42", "43"))
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc_$new(ctx, config, NULL, FALSE, FALSE, FALSE, client)

  private <- r6_private(obj)
  private$provisioned <- TRUE

  t1 <- obj$enqueue(sin(1))
  t2 <- obj$enqueue(cos(1))

  # Check a list works
  expect_identical(task_get_ids(c(t1,t2)),
                   c(t1$id, t2$id))

  # Check a vector of character ids works
  expect_identical(task_get_ids(c(t1$id,t2$id)),
                   c(t1$id, t2$id))

})


test_that("Can submit a group", {
  client <- list(
    submit = mockery::mock("1", "2", "3", "4"))
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc_$new(ctx, config, NULL, FALSE, FALSE, FALSE, client)
  private <- r6_private(obj)
  private$provisioned <- TRUE
  grp <- obj$lapply(1:4, sin)

  mockery::expect_called(client$submit, 4)
  expect_s3_class(grp, "task_bundle")

  expect_equal(obj$dide_id(grp), c("1", "2", "3", "4"))
})


## This is currently vesigial, but bound in with features of queuer/context
test_that("name expansion", {
  ids <- ids::random_id(4)
  nms <- c("a", "b", "c", "d")
  expect_equal(task_names(ids, NULL), ids)
  expect_equal(task_names(ids, nms),
               sprintf("%s (%s)", nms, ids))
  expect_error(task_names(ids, nms[-2]),
               "incorrect length names")
})


test_that("task id getter", {
  id <- ids::random_id()
  expect_error(task_get_id(1), "Can't determine task id")
  expect_equal(task_get_id(id), id)
  expect_equal(
    task_get_id(structure(list(id = id), class = "queuer_task")), id)
  expect_equal(
    task_get_id(structure(list(ids = id), class = "task_bundle")), id)
})


test_that("Can get all context packages", {
  root <- tempfile()
  repos <- c(didehpc = "https://mrc-ide.github.io/didehpc-pkgs")
  ctx <- context::context_save(root, packages = "foo")
  expect_equal(context_packages(ctx),
               list(packages = c("context", "foo"), repos = repos))
  expect_equal(context_packages(ctx, TRUE),
               list(packages = c("context", "rrq", "callr", "foo"),
                    repos = repos))
})


test_that("Can exclude base packages from install", {
  root <- tempfile()
  repos <- c(didehpc = "https://mrc-ide.github.io/didehpc-pkgs")
  ctx <- context::context_save(root, packages = c("base", "utils", "foo"))
  expect_equal(context_packages(ctx),
               list(packages = c("context", "foo"), repos = repos))
  expect_equal(context_packages(ctx, TRUE),
               list(packages = c("context", "rrq", "callr", "foo"),
                    repos = repos))
})


test_that("Can get all context packages where loaded is used", {
  root <- tempfile()
  repos <- c(didehpc = "https://mrc-ide.github.io/didehpc-pkgs")
  ctx <- context::context_save(
    root,
    packages = list(attached = "foo", loaded = "bar"))
  expect_equal(context_packages(ctx),
               list(packages = c("context", "foo", "bar"), repos = repos))
})


test_that("Can include sources", {
  root <- tempfile()
  repos <- c(CRAN = "https://cloud.r-project.org",
             didehpc = "https://mrc-ide.github.io/didehpc-pkgs")
  ctx <- context::context_save(
    root,
    packages = list(attached = "foo", loaded = "bar"),
    package_sources = conan::conan_sources("user/foo@ref"))
  expect_equal(context_packages(ctx),
               list(packages = c("context", "foo", "bar", "user/foo@ref"),
                    repos = repos))
})


test_that("Package provisioning interface logic is correct", {
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc(ctx, config, initialise = FALSE)

  private <- r6_private(obj)
  private$lib <- list(
    provision = mockery::mock(),
    check = mockery::mock(
      list(complete = FALSE, missing = "context", found = character(0)),
      list(complete = TRUE, missing = character(0), found = "context"),
      list(complete = TRUE, missing = character(0), found = "context")))
  expect_false(private$provisioned)

  expect_message(
    obj$provision_context(show_log = FALSE, show_progress = FALSE),
    "Running installation script on cluster")
  expect_true(private$provisioned)

  mockery::expect_called(private$lib$check, 1L)
  expect_equal(mockery::mock_args(private$lib$check)[[1]], list("context"))

  mockery::expect_called(private$lib$provision, 1L)
  repos <- c(didehpc = "https://mrc-ide.github.io/didehpc-pkgs")
  expect_equal(mockery::mock_args(private$lib$provision)[[1]],
               list("context", repos, "lazy", FALSE, FALSE, FALSE))

  expect_message(
    obj$provision_context(),
    "Nothing to install; try running with policy = 'upgrade'")
  expect_silent(
    obj$provision_context(quiet = TRUE))

  mockery::expect_called(private$lib$check, 3L)
  expect_equal(mockery::mock_args(private$lib$check)[[2]],
               mockery::mock_args(private$lib$check)[[1]])
  expect_equal(mockery::mock_args(private$lib$check)[[3]],
               mockery::mock_args(private$lib$check)[[1]])
  mockery::expect_called(private$lib$provision, 1L)
})


test_that("Package provisioning interface logic is correct", {
  config <- example_config()
  ctx <- context::context_save(file.path(config$workdir, "context"))
  obj <- queue_didehpc(ctx, config, initialise = FALSE)

  private <- r6_private(obj)
  private$lib <- list(
    provision = mockery::mock(),
    check = mockery::mock(list(complete = TRUE), cycle = TRUE))
  expect_false(private$provisioned)

  expect_message(
    obj$provision_context(),
    "Nothing to install; try running with policy = 'upgrade'")
  expect_true(private$provisioned)
})


test_that("Don't tell pkgdepends about installed packages if verylazy", {
  config <- example_config()
  packages <- c("context", "apple", "banana", "carrot", "durian", "eggplant")
  ctx <- context::context_save(file.path(config$workdir, "context"),
                               packages = packages[-1])
  obj <- queue_didehpc(ctx, config, initialise = FALSE)

  i <- c(3, 5)
  private <- r6_private(obj)
  private$lib <- list(
    provision = mockery::mock(),
    check = mockery::mock(
      list(complete = FALSE, missing = packages, found = character(0)),
      list(complete = FALSE, missing = packages[i], found = packages[-i]),
      list(complete = TRUE, missing = character(0), found = packages),
      cycle = TRUE))

  expect_false(private$provisioned)

  ## default is verylazy:
  expect_message(obj$provision_context(), "Running installation script")
  expect_equal(
    mockery::mock_args(private$lib$provision)[[1]][c(1, 3)],
    list(packages, "lazy"))

  expect_message(obj$provision_context(), "Running installation script")
  expect_equal(
    mockery::mock_args(private$lib$provision)[[2]][c(1, 3)],
    list(packages[i], "lazy"))

  expect_message(obj$provision_context(), "Nothing to install")
  mockery::expect_called(private$lib$provision, 2) # not called this time

  ## Lazy is slightly less lazy:
  expect_message(obj$provision_context("lazy"), "Running installation script")
  expect_equal(
    mockery::mock_args(private$lib$provision)[[3]][c(1, 3)],
    list(packages, "lazy"))

  expect_message(obj$provision_context("lazy"), "Running installation script")
  expect_equal(
    mockery::mock_args(private$lib$provision)[[4]][c(1, 3)],
    list(packages, "lazy")) # cf above; all packages now listed

  expect_message(obj$provision_context("lazy"), "Running installation script")
  expect_equal(
    mockery::mock_args(private$lib$provision)[[4]][c(1, 3)],
    list(packages, "lazy")) # cf above; actually called, all packages
})
