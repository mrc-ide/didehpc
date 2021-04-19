##' Overall cluster load for all clusters that you have access to.
##'
##' @title Overall cluster load
##'
##' @param credentials Your credentials
##'
##' @export
cluster_load <- function(credentials = NULL) {
  print(simple_client(credentials)$load_overall())
}

##' Test cluster login
##' @title Test cluster login
##' @param credentials Your credentials
##' @export
web_login <- function(credentials = NULL) {
  simple_client(credentials)
  invisible(TRUE)
}


r_versions <- function() {
  if (is.null(cache$r_versions)) {
    cache$r_versions <- web_client$new("public", login = FALSE)$r_versions()
  }
  cache$r_versions
}


simple_credentials <- function(credentials) {
  if (inherits(credentials, "didehpc_config")) {
    credentials <- credentials$credentials
  } else {
    credentials <- credentials %||%
      getOption("didehpc.credentials", NULL) %||%
      getOption("didehpc.username", NULL)
  }
  credentials
}


simple_client <- function(credentials, client = web_client) {
  credentials <- dide_credentials(simple_credentials(credentials), TRUE)
  client$new(credentials, login = TRUE)
}
