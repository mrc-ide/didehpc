prepare_path <- function(path, mappings) {
  if (!file.exists(path)) {
    stop("path does not exist: ", path)
  }
  path <- clean_path(normalizePath(path, mustWork=TRUE))
  ## TODO: currently assume that mappings does not end in a trailing slash.
  ## TODO: not sure about slash direction disagreements.
  for (m in mappings) {
    if (string_starts_with(path, m$path_local)) {
      m$rel <- substr(path, nchar(m$path_local) + 2L, nchar(path))
      return(m)
    }
  }
  stop("did not find network mapping for path ", path)
}

## It may be possible on many systems to infer the path_remote from
## the local path, which would be useful;
##   `mount` on linux
##   `net use` or the easier to parse variant on Windows
path_mapping <- function(name, path_local, path_remote, drive_remote) {
  assert_scalar_character(name)
  assert_scalar_character(path_local)
  assert_scalar_character(path_remote)
  assert_scalar_character(drive_remote)
  if (!grepl("^[A-Z]:", drive_remote)) {
    stop("drive_remote must be of the form 'X:'")
  }
  if (!file.exists(path_local)) {
    stop("Local mount point does not exist: ", path_local)
  }
  clean_path <- function(x) {
    sub("/+$", "", gsub("\\", "/", x, fixed=TRUE))
  }
  ret <-
    list(name=name,
         path_remote = clean_path(path_remote),
         path_local  = clean_path(normalizePath(path_local, mustWork=TRUE)),
         drive_remote = drive_remote)
  class(ret) <- "path_mapping"
  ret
}

as.character.path_mapping <- function(x) {
  if (is.null(x$rel)) {
    sprintf("(local) %s => %s => %s (remote)",
            x$path_local, x$path_remote, x$drive_remote)
  } else {
    sprintf("[rel: %s] (local) %s => %s => %s (remote)",
            x$rel, x$path_local, x$path_remote, x$drive_remote)
  }
}

print.path_mapping <- function(x, ...) {
  cat(paste0("<path mapping>: ", as.character(x), "\n"))
  invisible(x)
}

clean_path <- function(x) {
  sub("/+$", "", gsub("\\", "/", x, fixed=TRUE))
}
windows_path <- function(x) {
  gsub("/", "\\\\", x)
}

remote_path <- function(x) {
  windows_path(file.path(x$path_remote, x$rel, fsep="/"))
}

path_batch <- function(root, id) {
  file.path(root, "batch", paste0(id, ".bat"))
}

## These will change.
dide_home <- function(path, username) {
  assert_scalar_character(username)
  assert_character(path)
  paste0("\\\\fi--san02\\homes\\", username, "\\", gsub("/", "\\\\", path))
}
dide_temp <- function(path) {
  assert_character(path)
  paste0("\\\\fi--didef2\\tmp\\", "\\", gsub("/", "\\\\", path))
}
