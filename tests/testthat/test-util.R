context("util")

test_that("Sys_getenv cycles through environment variables", {
  withr::with_envvar(
    c(DIDEHPC_A="a", DIDEHPC_B="b"), {
      expect_equal(Sys_getenv(c("DIDEHPC_A", "DIDEHPC_B")), "a")
      expect_equal(Sys_getenv(c("DIDEHPC_B", "DIDEHPC_A")), "b")
      expect_equal(Sys_getenv(c("DIDEHPC_X", "DIDEHPC_A")), "a")
      expect_equal(Sys_getenv(c("DIDEHPC_A", "DIDEHPC_X")), "a")
      expect_equal(Sys_getenv(c("DIDEHPC_X", "DIDEHPC_Y")), NA_character_)
    })
})
