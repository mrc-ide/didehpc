example_root <- tempfile()
dir.create(example_root)
for (p in file.path(example_root, c("other", "home", "proj", "temp"))) {
  dir.create(p)
}

example_mounts <- function() {
  remote <- c("\\\\fi--didef3\\other",
              "\\\\fi--san03\\homes\\bob",
              "\\\\fi--didenas1\\Project",
              "\\\\fi--didef3\\tmp")
  local <- file.path(example_root, c("other", "home", "proj", "temp"))
  cbind(remote = remote, local = local)
}
