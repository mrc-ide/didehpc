detect_mount <- function() {
  if (is_windows()) {
    detect_mount_windows()
  } else {
    detect_mount_unix()
  }
}


detect_mount_windows <- function() {
  res <- call_wmic()
  path <- tempfile()
  writeLines(res, path)
  on.exit(file.remove(path))
  dat <- read.csv(path, stringsAsFactors = FALSE)
  cbind(remote = dat$RemoteName, local = dat$LocalName)
}


## TODO: No idea what spaces in the filenames will do here.  Nothing
## pretty, that's for sure.
detect_mount_unix <- function() {
  fail <- cbind(host = character(), path = character(), local = character(),
                remote = character())

  mount <- Sys.which("mount")
  if (mount == "") {
    return(fail)
  }

  type <- if (Sys.info()[["sysname"]] == "Darwin") "smbfs" else "cifs"
  re <- "//(?<user>[^@]*@)?(?<host>[^/]*)/(?<path>.*?)\\s+on\\s+(?<local>.+?) (?<extra>.+)$"
  dat <- system2(mount, c("-t", type), stdout = TRUE, stderr = FALSE)
  i <- grepl(re, dat, perl = TRUE)
  if (!all(i)) {
    ## This will be useful to see until I get this correct.
    warning("Ignoring mounts:\n", paste(re[!i], collapse = "\n"))
  }
  dat <- dat[i]

  if (length(dat) == 0L) {
    return(fail)
  }

  ## There are a couple of formats here.  On the VPN and with OSX
  ## (currently correlated) I see a //username@host/path format while
  ## on on the wired network and Linux I see //shorthost/path
  ##
  ## //(user@)?(host)(.dide.ic.ac.uk)?/(path)
  m <- rematch::re_match(re, dat)[, c("host", "path", "local"), drop = FALSE]

  host <- sub("\\.dide\\.ic\\.ac\\.uk$", "", m[, "host"])
  remote <- sprintf("\\\\%s\\%s", host, gsub("/", "\\\\", m[, "path"]))
  cbind(remote = remote, local = m[, "local"])
}


## Windows support:
call_wmic <- function() {
  windir <- Sys.getenv("WINDIR", "C:\\windows")

  methods <- c("csv",
               paste0(windir, "\\System32\\wbem\\en-US\\csv"),
               paste0(windir, "\\System32\\wbem\\en-GB\\csv"))

  for (meth in methods) {
    res <- detect_mount_windows_wmic(meth)
    if (res$success) {
      return(res$result)
    }
  }

  stop(sprintf("Error: Could not determine windows mounts using wmic\n%s",
               res$result))
}


detect_mount_windows_wmic <- function(formatstr) {
  format_csv <- sprintf('/format:"%s"', formatstr)
  path <- tempfile()
  res <- try(

    ## Using stdout = path does not work here, yielding a file that has
    ## embedded NULs and failing to be read.

    suppressWarnings(
      system2("wmic", c("netuse", "list", "brief", format_csv), stdout = TRUE)),
    silent = TRUE)

  status <- attr(res, "status")

  if (inherits(res, "try-error") || (!is.null(status) && status != 0L)) {
    list(success = FALSE, result = res)
  } else {
    list(success = TRUE, result = res)
  }
}
