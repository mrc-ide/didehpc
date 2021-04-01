example_root <- tempfile()
for (p in file.path(example_root, c("other", "home", "proj", "temp"), "sub")) {
  dir.create(p, FALSE, TRUE)
}

example_mounts <- function() {
  remote <- c("\\\\fi--didef3\\other",
              "\\\\fi--san03\\homes\\bob",
              "\\\\fi--didenas1\\Project",
              "\\\\fi--didef3\\tmp")
  local <- file.path(example_root, c("other", "home", "proj", "temp"))
  cbind(remote = remote, local = local)
}


example_config <- function() {
  mounts <- example_mounts()
  workdir <- file.path(example_root, "home", "sub")
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)
  withr::with_options(
    tmp_options_didehpc(),
    didehpc_config(credentials = "bob", workdir = workdir))
}
