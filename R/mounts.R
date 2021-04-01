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
         result = wmic_parse(system_intern_check(cmd))),
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


## Normalisation

## This function will detect home, temp and if the current working
## directory is not in one of those then continue on to detect the cwd
## too.
dide_detect_mount <- function(mounts, shares, home, temp,
                              workdir, username, remap_nas) {
  ret <- list()

  ## These two have a bit of logic, and will try to guess as best they
  ## can the right thing to do:
  ret$home <- dide_detect_mount_home(home, mounts, username)
  ret$temp <- dide_detect_mount_temp(temp, mounts)
  ret <- c(ret, dide_detect_mount_check_shares(shares))

  remote <- vcapply(ret, "[[", "drive_remote", USE.NAMES = FALSE)
  dups <- unique(remote[duplicated(remote)])
  if (length(dups) > 0L) {
    stop("Duplicate remote drive names: ", paste(dups, collapse = ", "))
  }

  ret <- dide_detect_mount_find_workdir(ret, workdir, mounts)

  if (remap_nas) {
    for (i in seq_along(ret)) {
      ret[[i]]$path_remote <-
               sub("^([/\\\\]{2}fi--didenas[1345])\\b", "\\1-app",
                   ret[[i]]$path_remote)
    }
  }
  ret
}


dide_detect_mount_home <- function(home, mounts, username) {
  if (is.null(home)) {
    ## Try to detect where home is currently mounted because Oliver
    ## keeps his on O.
    re <- "^\\\\\\\\(qdrive|fi--san03)(\\.dide\\.ic\\.ac\\.uk)?\\\\homes\\\\"
    is_home <- grepl(re, tolower(mounts[, "remote"]))
    if (sum(is_home) == 1L) {
      home <- path_mapping("home", mounts[is_home, "local"],
                           mounts[is_home, "remote"], "Q:")
    } else if (sum(is_home) > 1L) {
      stop(sprintf(
        "I am confused about your home directory; there are %d choices:\n%s",
        sum(is_home),
        paste(sprintf("   - %s => %s",
                      mounts[is_home, "local"],
                      mounts[is_home, "remote"]), collapse = "\n")))
    } else {
      ## For now, require that home is given otherwise there are a few
      ## things that might not work.  This might actually be OK but
      ## needs testing I think.  Test this with passing FALSE through
      ## and see what I can make break
      stop("I can't find your home directory!  Please mount it")
    }
  } else {
    if (identical(home, FALSE)) {
      home <- NULL
    } else if (is.character(home)) {
      home <- path_mapping("home", home, dide_home(username), "Q:")
    } else if (!inherits(home, "path_mapping")) {
      stop("Unexpected type for 'home'")
    }
  }

  home
}


dide_detect_mount_temp <- function(temp, mounts) {
  if (is.null(temp)) {
    is_temp <- string_starts_with(tolower(mounts[, "remote"]),
                                  "\\\\fi--didef3\\tmp")
    if (sum(is_temp) == 1L) {
      temp <- path_mapping("temp", mounts[is_temp, "local"],
                           dide_temp(""), "T:")
    } else if (sum(is_temp) > 1L) {
      stop(sprintf(
        "I am confused about your temp directory; there are %d choices:\n%s",
        sum(is_temp),
        paste(sprintf("   - %s => %s",
                      mounts[is_temp, "local"],
                      mounts[is_temp, "remote"]), collapse = "\n")))
    }
  } else {
    if (inherits(temp, "character")) {
      temp <- path_mapping("temp", temp, dide_temp(""), "T:")
    } else if (!inherits(temp, "path_mapping")) {
      stop("Unexpected type for 'temp'")
    }
  }

  temp
}


dide_detect_mount_check_shares <- function(shares) {
  if (length(shares) == 0) {
    return(NULL)
  }
  if (inherits(shares, "path_mapping")) {
    ret <- setNames(list(shares), shares$name)
  } else if (is.list(shares)) {
    if (!all(vlapply(shares, inherits, "path_mapping"))) {
      stop("All elements of 'shares' must be a path_mapping")
    }
    ret <- shares
  } else {
    stop("Invalid input for 'shares'")
  }
  ret
}


dide_detect_mount_find_workdir <- function(mapping, workdir, mounts) {
  if (is.null(workdir)) {
    workdir <- getwd()
  }
  ## TODO: this tolower should be windows/mac only, because case is
  ## important otherwise. However, we'll map against lowercase later.
  workdir <- tolower(workdir)

  mapped <- vcapply(mapping, "[[", "path_local")
  ok <- vlapply(tolower(mapped), string_starts_with, x = workdir)

  if (!any(ok)) {
    i <- (nzchar(mounts[, "local"]) &
          vlapply(tolower(mounts[, "local"]), string_starts_with, x = workdir))
    if (sum(i) == 1L) {
      drive <- available_drive(mapping, mounts[i, "local"])
      workdir_map <- path_mapping("workdir", mounts[i, "local"],
                                  mounts[i, "remote"], drive)
      mapping <- c(mapping, list(workdir = workdir_map))
    } else if (sum(i) > 1L) {
      stop("Having trouble determining the working directory mount point")
    } else { # sum(i) == 0
      ## NOTE: This needs to be checked later when firing up the
      ## queue, but I believe that it is.
      message(sprintf("Running out of place: %s is not on a network share",
                      workdir))
    }
  }

  mapping
}


## If we're mounting some local drive (not home/temp) then on windows
## we'll reflect the local drive letter. Otherwise on linux/mac we'll
## pick from a late letter.
available_drive <- function(shares, local_mount) {
  if (grepl("^[A-Za-z]:", local_mount)) {
    local_mount
  } else {
    used <- toupper(substr(vcapply(shares, "[[", "drive_remote"), 1, 1))
    paste0(setdiff(LETTERS[22:26], used)[[1L]], ":")
  }
}
