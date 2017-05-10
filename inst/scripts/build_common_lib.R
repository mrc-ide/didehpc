#!/usr/bin/env Rscript
provision_cluster <- function(cluster, installed_action = "upgrade") {
  PACKAGES <- c("context", # core package
                ## Additional didehpc system packages:
                "rrq", "queuer", "RPostgres", "redux", "RPostgreSQL",
                ## Support
                "storr",
                ## Large dependencies
                "BH", "stringi", "Rcpp")
  PATH_LIB <- "~/net/temp/didehpc"
  PATH_DRAT <- "~/Documents/Projects/epi/cluster/drat"

  url_context <- paste0("file://", normalizePath(PATH_DRAT))
  src <- provisionr::package_sources(repos = url_context)

  platform <- cran_platform(cluster)
  versions <- r_versions(cluster)
  versions <- lapply(unique(versions[, 1:2]),
                     function(x) max(versions[versions[, 1:2] == x]))

  for (v in versions) {
    message(sprintf("*** %s:%s", cluster, v))
    p <- context:::path_library(PATH_LIB, platform, v)
    res <- provisionr::provision_library(PACKAGES, p, platform, v, src,
                                         installed_action = installed_action,
                                         allow_missing = TRUE)
    if (!is.null(res$missing)) {
      config <- suppressMessages(
        didehpc_config(cluster = cluster, r_version = v))
      ans <- initialise_cluster_packages_build(res, config)
    }
  }
}
environment(provision_cluster) <- loadNamespace("didehpc")

provision_all <- function() {
  provision_cluster("fi--didemrchnb")
  provision_cluster("fi--didelxhn")
}

if (!interactive()) {
  provision_all()
}
