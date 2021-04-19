dide_username <- function(username) {
  check_username(username %||% prompt_username())
}


dide_credentials <- function(credentials, need_password) {
  if (is.character(credentials) && file.exists(credentials)) {
    credentials <- read_credentials(credentials)
  }
  if (is.null(credentials) || is.character(credentials)) {
    credentials <- list(username = dide_username(credentials))
  } else if (is.list(credentials)) {
    if (is.null(names(credentials)) || any(names(credentials) == "")) {
      stop("Credentials must be named")
    }
    extra <- setdiff(names(credentials), c("username", "password"))
    if (length(extra) > 0L) {
      stop("Unknown fields in credentials: ", paste(extra, collapse = ", "))
    }
    if (!("username" %in% names(credentials))) {
      stop("Missing fields in credentials: username")
    }
    credentials$username <- check_username(credentials$username)
  } else {
    stop("Unexpected type for credentials")
  }

  if (!is.null(credentials$password)) {
    assert_scalar_character(credentials$password)
  }

  if (need_password && is.null(credentials$password)) {
    credentials$password <- prompt_password(credentials$username)
  }

  if (!is.null(credentials$password)) {
    class(credentials$password) <- "password"
  }

  credentials
}


prompt_username <- function() {
  if (!interactive()) {
    stop("Credentials file needed for non-interactive use")
  }
  trimws(readline(prompt = "DIDE username: "))
}


prompt_password <- function(username) {
  if (!interactive()) {
    stop("Credentials file needed for non-interactive use")
  }
  getPass::getPass(sprintf("Enter DIDE password for %s: ", username))
}


check_username <- function(username) {
  assert_scalar_character(username)
  username <- sub("^DIDE\\\\", "", username)
  if (username == "") {
    stop("Invalid empty username")
  }
  username
}


## Format is
## username=<username>
## password=<password>
read_credentials <- function(filename) {
  dat <- strsplit(readLines(filename), "=")
  values <- trimws(vcapply(dat, "[[", 2L))
  nms <- trimws(vcapply(dat, "[[", 1L))
  setNames(as.list(values), nms)
}


##' @export
as.character.password <- function(x, ...) {
  "*******************"
}
