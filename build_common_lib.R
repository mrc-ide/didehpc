## Eventually this will support BH, which is the big nasty package
## that needs work.
devtools::load_all()
PACKAGES <- "context"
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
    provisionr::provision_library(PACKAGES, p, platform, v, src,
                                  installed_action = installed_action)
  }
}

if (FALSE) {
  provision_cluster("fi--didemrchnb")
}
