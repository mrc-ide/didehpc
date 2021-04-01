context("client (parse)")

test_that("can parse cancel return payloads", {
  expect_equal(
    client_parse_cancel("12345\tNOT_FOUND\n\n"),
    c("12345" = "NOT_FOUND"))
  expect_equal(
    client_parse_cancel("12345\tNOT_FOUND\n\n12346\tNOT_FOUND\n\n"),
    c("12345" = "NOT_FOUND", "12346" = "NOT_FOUND"))
})


test_that("can parse load return payloads", {
  txt <- read_lines("responses/load.txt")
  res <- client_parse_load_cluster(txt, "fi--dideclusthn")
  ## saveRDS(res, "responses/load.rds", version = 2L)
  expect_equal(res, readRDS("responses/load.rds"))
})
