context("can compile")

test_that("compile", {
  skip_on_travis()
  skip_if_not_installed("rcmdshlib")

  ## TODO: this triggers a fairly unpleasant and slow installation of
  ## BH.  It would be nice to be able to skip that by pointing things
  ## at a repo that already has it, I think, or to see if we can get
  ## stan to work without BH.
  owd <- prepare_didewin("rcmdshlib")
  on.exit(setwd(owd))

  ## TODO: provisionr is leaving a signature in the context which is
  ## not helpful; it's leading to errors and (worse) is going to mean
  ## that the context signature changes every time!  Doesn't seem to
  ## actually be happening though.

  path <- "context"
  src <- provisionr::package_sources(github = "richfitz/rcmdshlib")
  ctx <- context::context_save(path = path, packages = "rcmdshlib",
                               package_sources = src)
  config <- didewin::didewin_config(r_version = "3.3.2", rtools = TRUE)
  obj <- didewin::queue_didewin(ctx, config = config)

  expect_true(obj$config$rtools)
  expect_true(needs_rtools(obj$config, obj$context))

  t <- obj$enqueue(rcmdshlib::can_compile(verbose = TRUE))
  expect_true(t$wait(20))
})
