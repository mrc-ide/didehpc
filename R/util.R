## Base imports:
##' @importFrom stats setNames
##' @importFrom utils modifyList packageVersion read.csv
NULL

curl_insecure <- function() {
  httr::config(ssl_verifypeer = 0)
}


## Consider adopting the version in caTools
encode64 <- function(x) {
  if (x == "") {
    ""
  } else {
    storr::encode64(x, "+", "/")
  }
}


decode64 <- function(x) {
  storr::decode64(x, "+", "/")
}


modify_list <- function(x, val, name = deparse(substitute(val))) {
  extra <- setdiff(names(val), names(x))
  if (length(extra) > 0L) {
    stop(sprintf("Unknown elements in %s: %s",
                    name, paste(extra, collapse = ", ")))
    val <- val[setdiff(names(val), extra)]
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


time_checker <- function(timeout) {
  t0 <- Sys.time()
  timeout <- as.difftime(timeout, units = "secs")
  function() {
    Sys.time() - t0 > timeout
  }
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


`%||%` <- function(a, b) {
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


dquote <- function(x) {
  sprintf('"%s"', x)
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


Sys_which <- function(name) {
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
