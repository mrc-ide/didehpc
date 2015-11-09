##' @title Configuration
##' @param credentials Path to credentials file, or username
##' @param home Path to network home directory, on local system
##' @param temp Path to network temp directory, on local system
##' @param cluster Cluster to use
##' @export
didewin_config <- function(credentials=NULL, home=NULL, temp=NULL,
                           cluster=NULL) {
  defaults <- didewin_config_defaults()
  given <- list(credentials=credentials,
                home=home,
                temp=temp)
  dat <- modify_list(defaults,
                     given[!vapply(given, is.null, logical(1))])
  ## TODO: get_credentials() should prompt for username if it NULL
  ## NOTE: does *not* store (or request password)
  username <- get_credentials(dat$credentials, FALSE)$username
  ret <- list(cluster=match_value(dat$cluster, valid_clusters()),
              credentials=dat$credentials,
              username=username)
  ## Can offer support for additional mappings here, too, so long as
  ## they're absolute path mappings.
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
    cluster     = getOption("didewin.cluster",     valid_clusters()[[1]]),
    credentials = getOption("didewin.credentials", NULL),
    home        = getOption("didewin.home",        NULL),
    temp        = getOption("didewin.temp",        NULL))

  ## Extra shot for the windows users because we can do most of this
  ## automatically if they are a domain machine.  We might be able to
  ## get there with `mount` on Linux too.  Probably needs some work
  ## for cases where they're off the main network though.
  if (is_windows()) {
    if (is.null(defaults$home)) {
      defaults$home <- "Q:"
    }
    if (is.null(defaults$temp)) {
      defaults$home <- "T:"
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
