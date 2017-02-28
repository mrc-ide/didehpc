context("parallel")

test_that("parallel", {
  skip_on_travis()

  owd <- prepare_didehpc("parallel", "mysources-parallel.R")
  on.exit(setwd(owd))

  path <- "context"
  ctx <- context::context_save(path = path, packages = "ape",
                               sources = "mysources-parallel.R")
  config <- didehpc::didehpc_config(cores = 4L)
  obj <- didehpc::queue_didehpc(ctx, config = config)

  ## All good so far:
  t1 <- obj$enqueue(sessionInfo())
  res1 <- t1$wait(10, progress = FALSE)
  l1 <- t1$log()
  expect_equal(sum(l1$title == "cluster"), 5)
  expect_true("parallel" %in% names(res1$loadedOnly))

  ## Run a job:
  t2 <- obj$enqueue(list_parallel_pids())
  res2 <- t2$wait(10, progress = FALSE)
  expect_is(res2, "list")
  expect_equal(length(res2$parallel), 4)
  expect_false(res2$local %in% unlist(res2$parallel))

  ## Run a real job:
  t3 <- obj$enqueue(make_tree_par(10, 8))
  res3 <- t3$wait(10, progress = FALSE)
  expect_is(res3, "list")
  expect_equal(length(res3), 8)
  expect_is(res3[[5]], "phylo")
})
