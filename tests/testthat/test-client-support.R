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
