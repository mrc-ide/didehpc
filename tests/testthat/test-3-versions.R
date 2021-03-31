context("versions")

test_that("supported - defaults", {
  skip_on_travis()

  owd <- prepare_didehpc("versions")
  on.exit(setwd(owd))

  valid <- r_versions()
  for (i in seq_along(valid)) {
    v <- valid[[i]]
    with_mock(`base::getRversion` = function() v,
              expect_equal(didehpc_config()$r_version, v))
  }

  v_up <- max(valid)
  v_up[[1, 3]] <- v_up[[1, 3]] + 1L
  v_down <- numeric_version("3.2.0")
  v_msg <- numeric_version("3.2.9")

  with_mock(`base::getRversion` = function() v_up,
            expect_equal(didehpc_config()$r_version, max(valid)))
  with_mock(`base::getRversion` = function() v_down,
            expect_equal(didehpc_config()$r_version, min(valid)))
  with_mock(`base::getRversion` = function() v_msg,
            expect_equal(didehpc_config()$r_version, numeric_version("3.3.1")))


  expect_equal(didehpc_config(r_version = "3.3.1")$r_version,
               numeric_version("3.3.1"))
  expect_equal(didehpc_config(r_version = numeric_version("3.2.4"))$r_version,
               numeric_version("3.2.4"))

  expect_error(didehpc_config(r_version = "3.2.5")$r_version,
               "Unsupported R version: 3.2.5", fixed = TRUE)
})

test_that("versions", {
  skip_on_travis()

  owd <- prepare_didehpc("versions", "mysources.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources.R")

  ## Select the biggest version within each series to test
  valid <- r_versions()
  test <- lapply(unique(valid[, 1:2]),
                 function(x) max(valid[valid[, 1:2] == x]))
  for (v in test) {
    obj <- didehpc::queue_didehpc(ctx, config = didehpc_config(r_version = v))
    expect_true(file.exists(file.path(path, "lib", "windows", v[, 1:2], "ape")))
    t <- obj$enqueue(sessionInfo())
    res <- t$wait(10, progress = FALSE)
    expect_is(res, "sessionInfo")
    v_remote <- paste(res$R.version$major, res$R.version$minor, sep = ".")
    expect_equal(numeric_version(v_remote), v)
    expect_equal(names(res$otherPkgs), "ape")
  }
})
