prepare_path <- function(path, mappings, error=TRUE) {
  if (!file.exists(path)) {
    stop("path does not exist: ", path)
  }
  path <- clean_path(normalizePath(path, mustWork=TRUE))
  ## TODO: currently assume that mappings does not end in a trailing slash.
  ## TODO: not sure about slash direction disagreements.
  ## TODO: 'rel' is not relative to *our* working directory
  for (m in mappings) {
    if (string_starts_with(tolower(path), tolower(m$path_local))) {
      m$rel <- substr(path, nchar(m$path_local) + 2L, nchar(path))
      return(m)
    }
  }
  if (error) {
    stop("did not find network mapping for path ", path)
  } else {
    NULL
  }
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
as.character.path_mapping <- function(x, ...) {
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

detect_mount_fail <- function() {
  cbind(host=character(), path=character(), local=character())
}

## TODO: No idea what spaces in the filenames will do here.  Nothing
## pretty, that's for sure.
detect_mount_unix <- function() {
  mount <- Sys.which("mount")
  if (mount == "") {
    return(detect_mount_fail())
  }

  type <- if (Sys.info()[["sysname"]] == "Darwin") "smbfs" else "cifs"
  ## consider
  ##   mount -t cifs  # (linux)
  ##   mount -t smbfs # (osx)
  re <- "//(?<user>[^@]*@)?(?<host>[^/]*)/(?<path>.*?)\\s+on\\s+(?<local>.+?) (?<extra>.+)$"
  dat <- system2(mount, c("-t", type), stdout=TRUE, stderr=FALSE)
  i <- grepl(re, dat, perl=TRUE)
  if (!all(i)) {
    ## This will be useful to see until I get this correct.
    warning("Ignoring mounts:\n", paste(re[!i], collapse="\n"))
  }
  dat <- dat[i]

  if (length(dat) == 0L) {
    return(detect_mount_fail())
  }

  ## There are a couple of formats here.  On the VPN and with OSX
  ## (currently correlated) I see a //username@host/path format while
  ## on on the wired network and Linux I see //shorthost/path
  ##
  ## //(user@)?(host)(.dide.ic.ac.uk)?/(path)
  m <- rematch::re_match(re, dat)[, c("host", "path", "local"), drop=FALSE]

  host <- sub("\\.dide\\.ic\\.ac\\.uk$", "", m[, "host"])
  remote <- sprintf("\\\\%s\\%s", host, gsub("/", "\\\\", m[, "path"]))
  cbind(remote=remote, local=m[, "local"])
}

detect_mount_windows <- function() {
  windir <- Sys.getenv("WINDIR", "C:\\windows")
  format_csv <- sprintf('/format:"%s\\System32\\wbem\\en-US\\csv"', windir)

  ## Using stdout=path does not work here, yielding a file that has
  ## embedded NULs and failing to be read.
  path <- tempfile()
  tmp <- system2("wmic", c("netuse", "list", "brief", format_csv), stdout=TRUE)
  writeLines(tmp[-1], path)
  on.exit(file.remove(path))
  dat <- read.csv(path, stringsAsFactors=FALSE)

  cbind(remote=dat$RemoteName, local=dat$LocalName)
}

detect_mount <- function() {
  if (is_windows()) detect_mount_windows() else detect_mount_unix()
}
