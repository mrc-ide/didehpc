Sys_which <- function(name) {
  if (length(name) != 1L) {
    stop("'name' must be a scalar")
  }
  ret <- Sys.which(name)
  if (ret == "") {
    stop(sprintf("%s not found in $PATH", name))
  }
  ret
}

curl_insecure <- function() {
  httr::config(ssl_verifypeer=0)
}

## Consider adopting the version in caTools
encode64 <- function(x) {
  if (x == "") {
    ""
  } else {
    base64enc::base64encode(charToRaw(x))
  }
}
decode64 <- function(x) {
  rawToChar(base64enc::base64decode(x))
}
