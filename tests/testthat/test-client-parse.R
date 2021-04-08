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


test_that("can compute overall load", {
  d1 <- readRDS("responses/load.rds")
  d2 <- d1
  d2$overall$name <- "other"
  d2$overall$free <- 100
  d2$overall$used <- 116
  d2$overall$percent_used <- 54
  res <- client_parse_load_overall(list(d1, d2))
  expect_equal(res$cluster, "didehpc")
  expect_null(res$detail)
  expect_equal(
    res$summary,
    data.frame(name = c("fi--dideclusthn", "other"),
               free = c(203, 100),
               used = c(13, 116),
               total = 216,
               percent_used = c(6, 54),
               stringsAsFactors = FALSE))
  expect_equal(
    res$overall,
    list(name = "didehpc",
         free = 303,
         used = 129,
         total = 432,
         percent_used = 30))
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
  empty_time <- dide_time_parse(character())
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


test_that("Can parse status payload", {
  txt <- read_lines("responses/status.txt")
  res <- client_parse_status(txt)

  t1 <- c("20210402094710", "20210402093402", "20210402093346",
          "20210402092833", "20210402091124")
  t2 <- c("20210402094710", "20210402093401", "20210402093346",
          "20210402092833", "20210402091124")
  t3 <- c("20210402094720", "20210402093412", "20210402093356",
          "20210402092843", "20210402091134")

  cmp <- data.frame(
    dide_id = c("3490639", "3490638", "3490637", "3490636", "3490635"),
    name = c("test", "test_old", "test_old", "test_web", "test"),
    status = "COMPLETE",
    resources = "1 core",
    user = "bob",
    time_start = dide_time_parse(t1),
    time_submit = dide_time_parse(t2),
    time_end = dide_time_parse(t3),
    template = "GeneralNodes",
    stringsAsFactors = FALSE)
  expect_equal(res, cmp)
})


test_that("Handle parse failure", {
  txt <- read_lines("responses/status.txt")
  expect_error(
    client_parse_status(sub("bob\\s+", "", txt)),
    "Parse error; unexpected output from server")
})


test_that("Can parse submission of a single job", {
  expect_equal(
    client_parse_submit("Job has been submitted. ID: 3490639.\n", 1L),
    "3490639")
  expect_error(
    client_parse_submit("", 1L),
    "Job submission has likely failed; could be a login error")
  expect_error(
    client_parse_submit("Job has been submitted. ID: 3490639.\n", 2L),
    "Unexpected response length from server")
  expect_message(
    res <- client_parse_submit(
      "Job has been submitted. ID: 3490639.\nother", 1L),
    "Discarding additional response from server:\nother")
  expect_equal(res, "3490639")
})


test_that("Can parse logs", {
  txt <- read_lines("responses/log.txt")
  expect_equal(
    client_parse_log(txt),
    c("C:\\Users\\rfitzjoh>echo starting!",
      "starting!",
      "",
      "C:\\Users\\rfitzjoh>sleep 10",
      "",
      "C:\\Users\\rfitzjoh>echo done!",
      "done!"))
})


test_that("Can parse cancel payload", {
  expect_equal(
    client_parse_cancel("3490640\tOK\n"),
    setNames("OK", "3490640"))
  expect_equal(
    client_parse_cancel("3490640\tWRONG_STATE\n"),
    setNames("WRONG_STATE", "3490640"))
  expect_equal(
    client_parse_cancel("3490640\tWRONG_STATE\n"),
    setNames("WRONG_STATE", "3490640"))
  s <- paste0("3490640\tWRONG_STATE\n3490641\tNOT_FOUND\n\n",
              "3490642\tNOT_FOUND\n\n3490643\tNOT_FOUND\n\n")
  expect_equal(
    client_parse_cancel(s),
    c("3490640" = "WRONG_STATE",
      "3490641" = "NOT_FOUND",
      "3490642" = "NOT_FOUND",
      "3490643" = "NOT_FOUND"))
})
