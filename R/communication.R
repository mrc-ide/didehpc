##' Overall cluster load for all clusters that you have access to.
##'
##' @title Overall cluster load
##'
##' @param credentials Your credentials
##'
##' @export
cluster_load <- function(credentials = NULL) {
  if (inherits(credentials, "didehpc_config")) {
    credentials <- credentials$credentials
  }
  credentials <- dide_credentials(credentials)
  print(web_client$new(credentials)$load_overall())
}


r_versions <- function() {
  if (is.null(cache$r_versions)) {
    cache$r_versions <- web_client$new("public", login = FALSE)$r_versions()
  }
  cache$r_versions
}
