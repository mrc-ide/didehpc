## Eventually this will support BH, which is the big nasty package
## that needs work.
devtools::load_all()
PACKAGES <- c("context", # core package
              ## Additional didehpc system packages:
              "rrq", "queuer", "Rpostgres", "redux",
              ## Large dependencies
              "BH", "stringi", "Rcpp")
PATH <- "~/net/temp/didehpc"

provision_cluster <- function(cluster, installed_action = "skip") {
  url_context <-
    paste0("file://", normalizePath("~/Documents/Projects/epi/cluster/drat"))
  src <- provisionr::package_sources(repos = url_context)

  platform <- cran_platform(cluster)
  versions <- r_versions(cluster)
  versions <- lapply(unique(versions[, 1:2]),
                     function(x) max(versions[versions[, 1:2] == x]))

  for (v in versions) {
    message(sprintf("*** %s:%s", cluster, v))
    p <- context:::path_library(PATH, platform, v)
    res <- provisionr::provision_library(PACKAGES, p, platform, v, src,
                                         installed_action = installed_action,
                                         allow_missing = TRUE)
    if (!is.null(res$missing)) {
      config <- suppressMessages(didehpc_config(cluster = cluster))
      ans <- initialise_cluster_packages_build(res, config)
    }
  }
}

if (FALSE) {
  provision_cluster("fi--didemrchnb")
  provision_cluster("fi--didelxhn")
}
