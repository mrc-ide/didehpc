##' Collects configuration information.  Unfortunately there's a
##' fairly complicated process of working out what goes where so
##' documentation coming later.
##'
##' @section Resources and parallel computing:
##'
##' If you need more than one core per task (i.e., you want the each
##'   task to do some parallel processing \emph{in addition} to the
##'   parallelism between tasks) you can do that through the
##'   configuration options here.
##'
##' The \code{template} option choses among templates defined on the
##'   cluster.  If you select one of these then we will reserve an
##'   entire node \emph{unless} you also specify \code{cores}.
##'
##' If you specify \code{cores}, the HPC will queue your job until an
##'   appropriate number of cores appears for the selected template.
##'   This can leave your job queing forever (e.g., selecting 20 cores
##'   on a 16Core template) so be careful.  The \code{cores} option is
##'   most useful with the \code{GeneralNodes} template, which is the
##'   default.
##'
##' In either case, if more than 1 core is implied (either by using
##'   any template other than \code{GeneralNodes} or by specifying a
##'   \code{cores} value greater than 1) on startup, a \code{parallel}
##'   cluster will be started, using \code{parallel::makePSOCKcluster}
##'   and this will be registered as the default cluster.  The nodes
##'   will all have the appropriate context loaded and you can
##'   immediately use them with \code{parallel::clusterApply} and
##'   related functions by passing \code{NULL} as the first argument.
##'   The cluster will be shut down politely on exit, and logs will be
##'   output to the "workers" directory below your context root.
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
##' @param shares Optional additional share mappings.  Can either be a
##'   single path mapping (as returned by \code{\link{path_mapping}}
##'   or a list of such calls.
##'
##' @param template A job template.  On fi--dideclusthn this can be
##'   "GeneralNodes", "4Core" or "8Core", while on "fi--didemrchnb"
##'   this can be "GeneralNodes", "12Core" or "16Core", or "12and16Core".
##'
##' @param cores The number of cores to request.  This is really only
##'   useful when using the \code{GeneralNodes} template.  If
##'   specified, then we will request this many cores from the windows
##'   queuer.  If you request too many cores then your task will queue
##'   forever!  8 is the largest this should be on fi--didehusthn and
##'   16 on fi--didemrchnb (while there are 20 core nodes you may not
##'   have access to them).  If omitted then a single core is selected
##'   for the GeneralNodes template or the \emph{entire machine} for
##'   the other templates.
##'
##' @export
didewin_config <- function(credentials=NULL, home=NULL, temp=NULL,
                           cluster=NULL, build_server=NULL, shares=NULL,
                           template=NULL, cores=NULL) {
  defaults <- didewin_config_defaults()
  given <- list(credentials=credentials,
                home=home,
                temp=temp,
                cluster=cluster,
                build_server=build_server,
                shares=shares,
                template=template,
                cores=cores)
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
              build_server=dat$build_server,
              template=dat$template,
              cores=dat$cores)

  if (is.null(dat$shares)) {
    shares <- list()
  } else if (inherits(dat$shares, "path_mapping")) {
    shares <- list(dat$shares)
    names(shares) <- dat$shares$name
  } else if (is.list(dat$shares)) {
    if (!all(vlapply(dat$shares, inherits, "path_mapping"))) {
      stop("All elements of 'shares' must be a path_mapping")
    }
    shares <- dat$shares
  } else {
    stop("Invalid input for 'shares'")
  }
  if (!is.null(dat$home)) {
    shares$home <- path_mapping("home", dat$home, dide_home("", username), "Q:")
  }
  if (!is.null(dat$temp)) {
    shares$temp <- path_mapping("temp", dat$temp, dide_temp(""), "T:")
  }

  ret$resource <- check_resources(ret$cluster, ret$template, ret$cores)

  remote <- vcapply(shares, "[[", "drive_remote", USE.NAMES=FALSE)
  dups <- unique(remote[duplicated(remote)])
  if (length(dups) > 0L) {
    stop("Duplicate remote drive names: ", paste(dups, collapse=", "))
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
    build_server = getOption("didewin.build_server", "129.31.25.12"),
    shares       = getOption("didewin.shares",       NULL),
    template     = getOption("didewin.template",     "GeneralNodes"),
    cores        = getOption("didewin.cores",        NULL))

  ## Extra shot for the windows users because we can do most of this
  ## automatically if they are a domain machine.  We might be able to
  ## get there with `mount` on Linux too.  Probably needs some work
  ## for cases where they're off the main network though.
  if (is_windows()) {
    if (is.null(defaults$home)) {
      defaults$home <- "Q:/"
    }
    if (is.null(defaults$temp)) {
      defaults$temp <- "T:/"
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
      cat(sprintf(" - %s:\n", names(x)[[i]]))
      cat(paste(sprintf("    - %s: %s\n", names(x[[i]]),
                        vcapply(x[[i]], as.character)), collapse=""))
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

check_resources <- function(cluster, template, cores) {
  if (cluster == "fi--didemrchnb") {
    valid_templates <- c("GeneralNodes", "12Core", "12and16Core", "16Core")
  } else {
    valid_templates <- c("GeneralNodes", "4Core", "8Core")
  }
  assert_value(template, valid_templates)

  if (!is.null(cores)) {
    assert_scalar_integer(cores)
    max_cores <- if (cluster == "fi--didemrchnb") 16 else 8
    if (cores > max_cores) {
      stop(sprintf("Maximum number of cores for %s is %d", cluster, max_cores))
    }
    ret <- list(parallel=TRUE, count=cores, type="Cores")
  } else if (template == "GeneralNodes") {
    ret <- list(parallel=FALSE, count=1L, type="Cores")
  } else {
    ret <- list(parallel=TRUE, count=1L, "Nodes")
  }
  invisible(ret)
}

## TODO: This will eventually be configurable, but for now is assumed
## in a few places -- search for R_VERSION (all caps).
R_VERSION <- numeric_version("3.2.4")
R_BITS <- 64L
R_PLATFORM <- if (R_BITS == 64L) "x86_64-w64-mingw32" else "i386-w64-mingw32"
