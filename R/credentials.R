prompt_credentials <- function() {
  if (!interactive()) {
    stop("Credentials file needed for non-interactive use")
  }
  credentials <- trimws(readline(prompt = "DIDE username: "))
  if (credentials == "") {
    stop("Invalid empty username")
  }
  credentials
}

get_credentials <- function(credentials, need_password = TRUE) {
  if (is.null(credentials)) {
    credentials <- prompt_credentials()
  }
  if (is.list(credentials)) {
    ret <- check_credentials(credentials, need_password)
  } else if (is.character(credentials)) {
    if (file.exists(credentials)) {
      ret <- read_credentials(credentials, need_password)
    } else {
      ## Assume we have a username.
      ret <- list(username = credentials)
      if (need_password) {
        if (!interactive()) {
          stop("Credentials file needed for non-interactive use")
        }
        ret$password <- getPass::getPass(
          sprintf("Enter DIDE password for %s: ", ret$username))
      }
    }
  } else {
    stop("Unexpected type for credentials")
  }

  ret$username <- sub("^DIDE\\\\", "", ret$username)
  ret
}

## Format is
## username=<username>
## password=<password>
read_credentials <- function(filename, need_password) {
  dat <- strsplit(readLines(filename), "=")
  dat <- setNames(as.list(trimws(vapply(dat, "[[", character(1), 2L))),
                  trimws(vapply(dat, "[[", character(1), 1L)))
  check_credentials(dat, need_password)
}

check_credentials <- function(credentials, need_password) {
  if (is.null(names(credentials))) {
    stop("Credentials must be named")
  }
  extra <- setdiff(names(credentials), c("username", "password"))
  if (length(extra) > 0L) {
    stop("Unknown fields in credentials: ", paste(extra, collapse = ", "))
  }
  req <- c("username", if (need_password) "password")
  msg <- setdiff(req, names(credentials))
  if (length(msg) > 0L) {
    stop("Missing fields in credentials: ", paste(msg, collapse = ", "))
  }
  credentials
}
