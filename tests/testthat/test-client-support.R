context("client (support)")

test_that("Check cluster usage", {
  valid <- valid_clusters()
  expect_silent(client_check("fi--didemrchnb", valid))
  expect_error(
    client_check("fi--didemrchnb", character(0)),
    "You do not have access to any cluster")
  expect_error(
    client_check("fi--didemrchnb", "fi--dideclusthn"),
    "You do not have access to 'fi--didemrchnb'; try 'fi--dideclusthn'")
  expect_error(
    client_check("fi--didegpu", c("a", "b")),
    "You do not have access to 'fi--didegpu'; try one of 'a', 'b'")
})


test_that("Construct a submit body", {
  p <- "\\\\fi--host\\\\path"
  d <- client_body_submit(p, "name", "GeneralNodes", "fi--dideclusthn",
                          "Cores", 1, "1,2")
  expect_setequal(
    names(d),
    c("cluster", "template", "rc", "rt", "jn", "wd", "se", "so",
      "jobs", "dep", "hpcfunc"))
  expect_equal(d$cluster, encode64("fi--dideclusthn"))
  expect_equal(d$template, encode64("GeneralNodes"))
  expect_equal(d$rc, encode64("1"))
  expect_equal(d$rt, encode64("Cores"))
  expect_equal(d$wd, "") # we might set this in future though
  expect_equal(d$se, "") # we might set this in future though
  expect_equal(d$so, "") # we might set this in future though
  expect_equal(d$jobs, encode64(sprintf('call "%s"', p)))
  expect_equal(d$dep, encode64("1,2"))
  expect_equal(d$hpcfunc, "submit")
})


test_that("submission body validates path", {
  p <- "\\\\fi--host\\\\path"
  expect_error(
    client_body_submit(unix_path(p), "name", "GeneralNodes",
                       "fi--dideclusthn", "Cores", 1),
    "All paths must be Windows network paths")
})


test_that("Construct a cancel body", {
  cluster <- "fi--dideclusthn"
  expect_equal(
    client_body_cancel("123456", cluster),
    list(cluster = encode64(cluster),
         hpcfunc = encode64("cancel"),
         c123456 = "123456"))
  expect_equal(
    client_body_cancel(c("123456", "234567"), cluster),
    list(cluster = encode64(cluster),
         hpcfunc = encode64("cancel"),
         c123456 = "123456",
         c234567 = "234567"))
  expect_error(
    client_body_cancel(character(0), cluster),
    "Need at least one task to cancel")
})
