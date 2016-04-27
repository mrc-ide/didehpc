prepare_path <- function(path, mappings) {
  if (!file.exists(path)) {
    stop("path does not exist: ", path)
  }
  path <- clean_path(normalizePath(path, mustWork=TRUE))
  ## TODO: currently assume that mappings does not end in a trailing slash.
  ## TODO: not sure about slash direction disagreements.
  for (m in mappings) {
    if (string_starts_with(tolower(path), tolower(m$path_local))) {
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

##' Describe a path mapping for use when setting up jobs on the cluster.
##' @title Describe a path mapping
##'
##' @param name Name of this map.  Can be anything at all, and is used
##'   for information purpopses only.
##'
##' @param path_local The point where the drive is attached locally.
##'   On Windows this will be something like "Q:/", on Mac something
##'   like "/Volumes/mountname", and on Linux it could be anything at
##'   all, depending on what you used when you mounted it (or what is
##'   written in \code{/etc/fstab})
##'
##' @param path_remote The \emph{network path} for this drive.  It
##'   will look something like \code{\\\\fi--didef2\\tmp\\}.
##'   Unfortunately backslashes are really hard to get right here and
##'   you will need to use twice as many as you expect (so \emph{four}
##'   backslashes at the beginning and then two for each separator.
##'   If this makes you feel bad know that you are not alone:
##'   https://xkcd.com/1638 -- alternatively you may use forward
##'   slashes in place of backslashes (e.g. //fi--didef2/tmp)
##'
##' @param drive_remote The place to mount the drive on the cluster.
##'   We're probably going to mount things at Q: and T: already so
##'   don't use those.  And things like C: are likely to be used.
##'   Perhaps there are some guidelines for this somewhere?
##'
##' @export
##' @author Rich FitzJohn
path_mapping <- function(name, path_local, path_remote, drive_remote) {
  assert_scalar_character(name)
  assert_scalar_character(path_local)
  assert_scalar_character(path_remote)
  assert_scalar_character(drive_remote)
  if (!grepl("^[A-Za-z]:$", drive_remote)) {
    stop("drive_remote must be of the form 'X:'")
  }
  if (grepl("^[A-Za-z]:$", path_local)) {
    path_local <- paste0(path_local, "/")
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

##' @export
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

file_path <- function(...) {
  paths <- list(...)
  paths <- paths[!vapply(paths, is.null, logical(1))]
  do.call("file.path", paths, quote=TRUE)
}
path_batch <- function(root, id=NULL) {
  if (!is.null(id)) {
    id <- paste0(id, ".bat")
  }
  file_path(root, "batch", id)
}
path_logs <- function(root, id=NULL) {
  file_path(root, "logs", id)
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
