## Usage:
##
## source("https://mrc-ide.github.io/didehpc/install")
## source("https://mrc-ide.github.io/didehpc/install#extras")
## source("https://mrc-ide.github.io/didehpc/install#upgrade")
## source("https://mrc-ide.github.io/didehpc/install#upgrade,extras")
##
## Alternatively, we could put this on mrcdata:
##
## source("https://mrcdata.ic.ac.uk/hpc/install")
## source("https://mrcdata.ic.ac.uk/hpc/install#extras")
## source("https://mrcdata.ic.ac.uk/hpc/install#upgrade")
## source("https://mrcdata.ic.ac.uk/hpc/install#upgrade,extras")
##
## Or, finally, put this into the shared drive somewhere and source
## the file directly.  That would require that people know where the
## temporary drive is on their computer though.
local({
  found <- FALSE
  for (call in rev(sys.calls())) {
    if (identical(call[[1]], quote(source))) {
      found <- TRUE
      break
    }
  }
  if (found) {
    file <- match.call(source, call)$file
    args <- strsplit(sub("[^#]*#", "", file), ",", fixed = TRUE)[[1]]
    i <- c("replace", "upgrade_all", "upgrade", "skip")
    installed_action <- i[[min(c(which(i %in% args), length(i)))]]
    extras <- "extras" %in% args
  } else {
    installed_action <- "skip"
    extras <- FALSE
  }

  log <- function(topic, value) {
    message(trimws(sprintf("[ %-9s ]  %s", topic, value)))
  }

  ns <- loadedNamespaces()
  oo <- options("repos")
  on.exit({
    options(oo)
    tryCatch(error = function(e) NULL,
           for (x in rev(setdiff(loadedNamespaces(), ns))) {
             unloadNamespace(x)
           })
  })

  installed <- .packages(TRUE)
  if (!("drat" %in% installed)) {
    log("install", "drat")
    install.packages("drat")
  }

  drat:::add("mrc-ide")
  if (!("provisionr" %in% installed)) {
    log("install", "provisionr")
    install.packages("provisionr")
  }
  packages <- c("didehpc", "buildr", "syncr")
  if (extras) {
    packages <- c("rrq", "RPostgres")
  }
  log("install", paste(packages, collapse = ", "))
  lib <- .libPaths()[[1L]]
  provisionr::provision_library(
    packages, lib, installed_action = installed_action)
  log("done", "")
  invisible(NULL)
})
