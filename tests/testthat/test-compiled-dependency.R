context("compiled dependency")

test_that("compiled dependency", {
  skip_on_travis()

  owd <- prepare_didewin("compiled")
  on.exit(setwd(owd))

  path <- "context"
  src <- provisionr::package_sources(github = "richfitz/seagull")
  ctx <- context::context_save(path = path, packages = "seagull",
                               package_sources = src)
  obj <- didewin::queue_didewin(ctx)

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  expect_equal(names(res$otherPkgs), "seagull")

  msg <- capture_messages(obj <- didewin::queue_didewin(ctx))
  expect_false(any(grepl("drat", msg)))
})
