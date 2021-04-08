## Base imports:
##' @importFrom stats setNames
##' @importFrom utils modifyList packageVersion read.csv
NULL

## Consider adopting the version in caTools
encode64 <- function(x) {
  if (x == "") {
    ""
  } else {
    storr::encode64(x, "+", "/")
  }
}


decode64 <- function(x) {
  ## There's a bug in storr we should fix, but we can work around it
  ## easily enough
  storr::decode64(chartr("+/", "-_", x))
}


modify_list <- function(x, val, name = deparse(substitute(val))) {
  extra <- setdiff(names(val), names(x))
  if (length(extra) > 0L) {
    stop(sprintf("Unknown elements in %s: %s",
                    name, paste(extra, collapse = ", ")))
  }
  modifyList(x, val)
}


is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}


is_linux <- function() {
  Sys.info()[["sysname"]] == "Linux"
}


string_starts_with <- function(x, y) {
  substr(x, 1, nchar(y)) == y
}


drop_blank <- function(x) {
  sub("^\n", "", gsub("\n[[:space:]]*\n", "\n", x))
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}


`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


is_directory <- function(path) {
  file.exists(path) && file.info(path, extra_cols = FALSE)[["isdir"]]
}


hostname <- function() {
  Sys.info()[["nodename"]]
}


read_lines <- function(...) {
  paste(readLines(...), collapse = "\n")
}


squote <- function(x) {
  sprintf("'%s'", x)
}


backup <- function(filename, verbose = TRUE) {
  if (file.exists(filename)) {
    pat <- sprintf("%s\\.([0-9]+)", basename(filename))
    found <- dir(dirname(filename), pattern = pat)
    if (length(found) > 0) {
      n <- max(as.integer(sub(pat, "\\1", found))) + 1
    } else {
      n <- 1
    }
    dest <- sprintf("%s.%d", filename, n)
    if (verbose) {
      message(sprintf("Copying %s -> %s", filename, basename(dest)))
    }
    file.copy(filename, dest)
  }
}


from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}


sys_which <- function(name) {
  ret <- Sys.which(name)
  if (ret == "") {
    stop(sprintf("%s not found in $PATH", name))
  }
  ret
}


system_intern_check <- function(...) {
  res <- suppressWarnings(system(..., intern = TRUE))
  status <- attr(res, "status", exact = TRUE)
  if (!is.null(status) && status > 0) {
    stop("Error running command")
  }
  res
}


httr_text <- function(r) {
  httr::content(r, as = "text", encoding = "UTF-8")
}


dide_time_parse <- function(x) {
  ## YYYYMMDDHHMMSS
  ## 20151109170805
  strptime(x, "%Y%m%d%H%M%S")
}


readlines_if_exists <- function(path, ...) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readLines(path, ...)
}


glue_whisker <- function(template, data) {
  transformer <- function(...) {
    ## This transformer prevents a NULL entry destroying the string
    glue::identity_transformer(...) %||% ""
  }
  glue::glue(template, .envir = data, .open = "{{", .close = "}}",
             .trim = FALSE, .transformer = transformer)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}
