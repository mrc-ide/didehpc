context("linux (complex dependencies)")

test_that("linux2", {
  ## TODO: this is not going to work in the cases where a package has
  ## complex dependencies; I should update buildr to respect Remotes:
  ## fields in DESCRIPTION, which would be the easiest way of
  ## supporting that I think (though discounting packages that are
  ## already uploaded would be necessary).
  skip_on_travis()
  skip_if_not_installed("buildr")

  owd <- prepare_didehpc("linux2", "mysources.R")
  on.exit(setwd(owd))

  config <- didehpc_config(cluster = "fi--didelxhn")
  github <- c("jeffeaton/anclik/anclik",
              "jeffeaton/epp",
              "jeffeaton/eppspectrum@allcausemx",
              "s-u/fastmatch")
  src <- provisionr::package_sources(github = github)
  root <- "context"
  ctx <- context::context_save(root,
                               packages = c("fastmatch", "eppspectrum"),
                               package_sources = src)
  obj <- didehpc::queue_didehpc(ctx, config = config)

  t <- obj$enqueue(sessionInfo())
  res <- t$wait(10, progress = FALSE)
  res
})
