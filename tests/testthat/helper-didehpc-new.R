example_mounts <- function(root) {
  remote <- c("\\\\fi--didef3\\other",
              "\\\\fi--san03\\homes\\bob",
              "\\\\fi--didenas1\\Project",
              "\\\\fi--didef3\\tmp")
  local <- file.path(root, c("other", "home", "proj", "temp"))
  for (p in file.path(local, "sub")) {
    dir.create(p, FALSE, TRUE)
  }
  cbind(remote = remote, local = local)
}


example_config <- function(..., root = tempfile()) {
  mounts <- example_mounts(root)
  workdir <- file.path(root, "home", "sub")
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)
  withr::with_options(
    tmp_options_didehpc(),
    didehpc_config(credentials = "bob", workdir = workdir, ...))
}


example_credentials <- function(online = FALSE) {
  if (online) {
    path <- "~/.smbcredentials"
    if (!file.exists(path)) {
      testthat::skip("credential file not found")
    }
    dide_credentials(path, TRUE)
  } else {
    dide_credentials(list(username = "bob", password = "secret"), TRUE)
  }
}


mock_response <- function(code, ..., url = NULL, content = NULL) {
  dat <- list(status_code = code,
              url = url %||% "http://example.com/",
              ...)
  if (is.character(content)) {
    dat$content <- charToRaw(paste(content, collapse = "\n"))
  } else {
    dat$content <- content
  }
  class(dat) <- "response"
  dat
}


r6_private <- function(x) {
  x[[".__enclos_env__"]]$private
}


password <- function(x) {
  structure(x, class = "password")
}


skip_if_no_redis <- function() {
  tryCatch(
    redux::hiredis()$PING(),
    error = function(e) testthat::skip("redis not available"))
  invisible(NULL)
}


same_path <- function(a, b) {
  normalizePath(a, "/", TRUE) == normalizePath(b, "/", TRUE)
}
