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


test_that("can parse R versions", {
  txt <- paste(
    '{"software": [',
    '  {"name": "R", "version": "3.6.0"},',
    '  {"name": "R", "version": "3.6.1"},',
    '  {"name": "other", "version": "1.2.3"},',
    '  {"name": "R", "version": "3.6.3"},',
    '  {"name": "R", "version": "4.0.2"},',
    '  {"name": "R", "version": "4.0.3"}',
    ']}', collapse = "\n")
  expect_equal(
    client_parse_r_versions(txt),
    numeric_version(c("3.6.0", "3.6.1", "3.6.3", "4.0.2", "4.0.3")))
})


test_that("can parse headnodes payload", {
  txt <- "fi--dideclusthn\nfi--didemrchnb\nfi--didelxhn\n"
  expect_equal(
    client_parse_headnodes(txt),
    c("fi--dideclusthn", "fi--didemrchnb"))
})


test_that("Can parse empty status payload", {
  empty_time <- Sys.time()[0]
  res <- client_parse_status("")
  expect_equal(res,
               data.frame(dide_id = character(0),
                          name = character(0),
                          status = character(0),
                          resources = character(0),
                          user = character(0),
                          time_start = empty_time,
                          time_submit = empty_time,
                          time_end = empty_time,
                          template = character(0),
                          stringsAsFactors = FALSE))
})
