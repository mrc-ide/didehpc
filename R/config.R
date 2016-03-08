##' Collects configuration information.  Unfortunately there's a
##' fairly complicated process of working out what goes where so
##' documentation coming later.
##'
##' @title Configuration
##'
##' @param credentials Either a list with elements username, password,
##'   or a path to a file containing lines \code{username=<username>}
##'   and \code{password=<password>} or your username (in which case
##'   you will be prompted graphically for your password).
##'
##' @param home Path to network home directory, on local system
##'
##' @param temp Path to network temp directory, on local system
##'
##' @param cluster Name of the cluster to use (one of
##' \code{\link{valid_clusters}()})
##'
##' @export
didewin_config <- function(credentials=NULL, home=NULL, temp=NULL,
                           cluster=NULL, build_server=NULL) {
  defaults <- didewin_config_defaults()
  given <- list(credentials=credentials,
                home=home,
                temp=temp,
                cluster=cluster,
                build_server=build_server)
  dat <- modify_list(defaults,
                     given[!vapply(given, is.null, logical(1))])
  ## NOTE: does *not* store (or request password)
  username <- get_credentials(dat$credentials, FALSE)$username
  if (is.null(dat$credentials)) {
    dat$credentials <- username
  }
  ret <- list(cluster=match_value(dat$cluster, valid_clusters()),
              credentials=dat$credentials,
              username=username,
              build_server=dat$build_server)
  ## Can offer support for additional mappings here, too, so long as
  ## they're absolute path mappings.
  ##
  ## TODO: check that all paths point at different remote paths, and
  ## different drives.  More of an issue when we accept generic
  ## mappings.
  shares <- list()
  if (!is.null(dat$home)) {
    shares$home <- path_mapping("home", dat$home, dide_home("", username), "Q:")
  }
  if (!is.null(dat$temp)) {
    shares$temp <- path_mapping("temp", dat$temp, dide_temp(""), "T:")
  }
  ret$shares <- shares

  class(ret) <- "didewin_config"
  ret
}

##' @param ... arguments to \code{didewin_config}
##' @export
##' @rdname didewin_config
didewin_config_global <- function(...) {
  opts <- list(...)
  if (length(opts) > 0L) {
    nms <- names(opts)
    if (is.null(nms) || any(nms == "")) {
      stop("All options must be named")
    }
    extra <- setdiff(nms, names(formals(didewin_config)))
    if (length(extra)) {
      stop("Unknown options: ", paste(extra, collapse=", "))
    }
    names(opts) <- paste0("didewin.", nms)
    oo <- options(opts)
    on.exit(options(oo))
    ## check that we're ok:
    tmp <- didewin_config()
    on.exit()
    invisible(oo)
  } else {
    invisible(list())
  }
}

didewin_config_defaults <- function() {
  defaults <- list(
    cluster      = getOption("didewin.cluster",      valid_clusters()[[1]]),
    credentials  = getOption("didewin.credentials",  NULL),
    home         = getOption("didewin.home",         NULL),
    temp         = getOption("didewin.temp",         NULL),
    build_server = getOption("didewin.build_server", "129.31.25.12"))

  ## Extra shot for the windows users because we can do most of this
  ## automatically if they are a domain machine.  We might be able to
  ## get there with `mount` on Linux too.  Probably needs some work
  ## for cases where they're off the main network though.
  if (is_windows()) {
    if (is.null(defaults$home)) {
      defaults$home <- "Q:/"
    }
    if (is.null(defaults$temp)) {
      defaults$home <- "T:/"
    }
    if (is.null(defaults$credentials)) {
      defaults$credentials <- Sys.getenv("USERNAME")
    }
  }
  defaults
}

##' @export
print.didewin_config <- function(x, ...) {
  cat("<didewin_config>\n")
  for (i in seq_along(x)) {
    if (is.atomic(x[[i]])) {
      cat(sprintf(" - %s: %s\n", names(x)[[i]], x[[i]]))
    } else if (is.list(x[[i]])) {
      cat(sprintf(" - %s:\n", names(x)[[i]], length(x[[i]])))
      for (j in x[[i]]) {
        cat(sprintf("   - %s\n", as.character(j)))
      }
    }
  }
  invisible(x)
}

##' Valid cluster names
##' @title Valid DIDE clusters
##' @export
valid_clusters <- function() {
  c("fi--dideclusthn", "fi--didemrchnb")
}

## TODO: This will eventually be configurable, but for now is assumed
## in a few places -- search for R_VERSION (all caps).
R_VERSION <- numeric_version("3.2.3")
R_BITS <- 64L
R_PLATFORM <- if (R_BITS == 64L) "x86_64-w64-mingw32" else "i386-w64-mingw32"
