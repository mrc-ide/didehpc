detect_mount <- function() {
  if (is_windows()) {
    detect_mount_windows()
  } else {
    detect_mount_unix()
  }
}


detect_mount_windows <- function() {
  windir <- Sys.getenv("WINDIR", "C:\\windows")
  methods <- c("csv",
               paste0(windir, "\\System32\\wbem\\en-US\\csv"),
               paste0(windir, "\\System32\\wbem\\en-GB\\csv"))

  for (meth in methods) {
    res <- wmic_call(meth)
    if (res$success) {
      return(res$result)
    }
  }

  stop("Could not determine windows mounts using wmic\n", res$result)
}


## TODO: No idea what spaces in the filenames will do here.  Nothing
## pretty, that's for sure.
detect_mount_unix <- function() {
  mount <- Sys_which("mount")
  type <- if (Sys.info()[["sysname"]] == "Darwin") "smbfs" else "cifs"

  re <- "//(?<user>[^@]*@)?(?<host>[^/]*)/(?<path>.*?)\\s+on\\s+(?<local>.+?) (?<extra>.+)$"
  dat <- system2(mount, c("-t", type), stdout = TRUE, stderr = FALSE)

  i <- grepl(re, dat, perl = TRUE)
  if (!all(i)) {
    ## This will be useful to see if the above regex becomes wrong
    warning("Ignoring mounts:\n", paste(dat[!i], collapse = "\n"),
            immediate. = TRUE)
  }
  dat <- dat[i]

  if (length(dat) == 0L) {
    return(cbind(remote = character(), local = character()))
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
wmic_call <- function(formatstr) {
  ## ordinarily we'd use system2 but that writes a string that can't
  ## be parsed under Rgui due to odd encoding.
  ## https://stackoverflow.com/q/61067574
  ## Using system() does not seem to suffer the same problem
  cmd <- sprintf('wmic netuse list brief /format:"%s"', formatstr)
  res <- tryCatch(
    list(success = TRUE,
         result = wmic_parse(system_internal_check(cmd))),
    error = function(e) list(success = FALSE, result = e$message))
}


wmic_parse <- function(x) {
  tmp <- tempfile()
  writeLines(x, tmp)
  on.exit(unlink(tmp))
  dat <- read.csv(tmp, stringsAsFactors = FALSE)
  expected <- c("RemoteName", "LocalName")
  msg <- setdiff(expected, names(dat))
  if (length(msg) > 0) {
    stop("Failed to find expected names in wmic output: ",
         paste(msg, collapse = ", "))
  }
  cbind(remote = dat$RemoteName, local = dat$LocalName)
}
