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
##'   Alternatively if \code{wholenode} is specified this overrides
##'   the logic here.
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
##' @param build_server Currently ignored, but will soon be a windows
##'   build server for bespoke binary packages.
##'
##' @param shares Optional additional share mappings.  Can either be a
##'   single path mapping (as returned by \code{\link{path_mapping}}
##'   or a list of such calls.
##'
##' @param template A job template.  On fi--dideclusthn this can be
##'   "GeneralNodes", "4Core" or "8Core", while on "fi--didemrchnb"
##'   this can be "GeneralNodes", "12Core" or "16Core", "12and16Core",
##'   "20Core", or "24Core".  See the main cluster documentation if
##'   you tweak these parameters, as you may not have permission to
##'   use all templates (and if you use one that you don't have
##'   permission for the job will fail).
##'
##' @param cores The number of cores to request.  This is mostly
##'   useful when using the \code{GeneralNodes} template.  If
##'   specified, then we will request this many cores from the windows
##'   queuer.  If you request too many cores then your task will queue
##'   forever!  8 is the largest this should be on fi--didehusthn and
##'   16 on fi--didemrchnb (while there are 20 core nodes you may not
##'   have access to them).  If omitted then a single core is selected
##'   for the GeneralNodes template or the \emph{entire machine} for
##'   the other templates (unless modified by \code{wholenode}).
##'
##' @param wholenode Request the whole node?  This will default to
##'   \code{TRUE} if any template other than \code{GeneralNodes} is
##'   selected.
##'
##' @param parallel Should we set up the parallel cluster?  Normally
##'   if more than one core is implied (via the \code{cores} argument,
##'   by picking a template other than \code{GeneralNodes} or by using
##'   \code{wholenode}) then a parallel cluster will be set up (see
##'   Details).  If \code{parallel} is set to \code{FALSE} then this
##'   will not occur.  This might be useful in cases where you want to
##'   manage your own job level parallelism (e.g. using OpenMP) or if
##'   you're just after the whole node for the memory).
##'
##' @param hpctools Use HPC tools if available?
##'
##' @export
didewin_config <- function(credentials=NULL, home=NULL, temp=NULL,
                           cluster=NULL, build_server=NULL, shares=NULL,
                           template=NULL, cores=NULL,
                           wholenode=NULL, parallel=NULL, hpctools=NULL) {
  defaults <- didewin_config_defaults()
  given <- list(credentials=credentials,
                home=home,
                temp=temp,
                cluster=cluster,
                build_server=build_server,
                shares=shares,
                template=template,
                cores=cores,
                wholenode=wholenode,
                parallel=parallel,
                hpctools=hpctools)
  dat <- modify_list(defaults,
                     given[!vapply(given, is.null, logical(1))])
  ## NOTE: does *not* store (or request password)
  username <- get_credentials(dat$credentials, FALSE)$username
  if (is.null(dat$credentials)) {
    dat$credentials <- username
  }

  cluster <- match_value(dat$cluster, valid_clusters())
  shares <- dide_detect_mount(dat$home, dat$temp, dat$shares, username)
  resource <- check_resources(cluster, dat$template, dat$cores,
                              dat$wholenode, dat$parallel)

  if (isTRUE(dat$hpctools)) {
    if (!has_hpctools()) {
      stop("HPC tools are requested but not found")
    }
  } else {
    dat$hpctools <- FALSE
  }

  ret <- list(cluster=cluster,
              credentials=dat$credentials,
              username=username,
              build_server=dat$build_server,
              template=dat$template,
              cores=dat$cores,
              wholenode=dat$wholenode,
              parallel=dat$parallel,
              hpctools=dat$hpctools,
              resource=resource,
              shares=shares)

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
    if (!all(vlapply(opts, is.null))) {
      ## check that we're ok, if we actually set anything.
      tmp <- didewin_config()
    }
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
    cores        = getOption("didewin.cores",        NULL),
    wholenode    = getOption("didewin.wholenode",    NULL),
    parallel     = getOption("didewin.parallel",     NULL),
    hpctools     = getOption("didewin.hpctools",     FALSE))

  if (is.null(defaults$credentials)) {
    username <- getOption("didewin.username", NULL)
    if (!is.null(username)) {
      defaults$credentials <- username
    }
  }

  ## Extra shot for the windows users because we get the username
  ## automatically if they are on a domain machine.
  if (is_windows() && is.null(defaults$credentials)) {
    defaults$credentials <- Sys.getenv("USERNAME")
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

check_resources <- function(cluster, template, cores, wholenode, parallel) {
  if (cluster == "fi--didemrchnb") {
    valid_templates <- c("GeneralNodes", "12Core", "12and16Core", "16Core",
                         "24Core")
  } else {
    valid_templates <- c("GeneralNodes", "4Core", "8Core")
  }
  assert_value(template, valid_templates)

  if (!is.null(cores)) {
    if (isTRUE(wholenode)) {
      stop("Cannot specify both wholenode and cores")
    }
    assert_scalar_integer(cores)
    max_cores <- if (cluster == "fi--didemrchnb") 16 else 8
    if (cores > max_cores) {
      stop(sprintf("Maximum number of cores for %s is %d", cluster, max_cores))
    }
    ret <- list(parallel=parallel %||% TRUE, count=cores, type="Cores")
  } else if (template != "GeneralNodes" || isTRUE(wholenode)) {
    ret <- list(parallel=parallel %||% TRUE, count=1L, type="Nodes")
  } else {
    ret <- list(parallel=parallel %||% FALSE, count=1L, type="Cores")
  }
  invisible(ret)
}

## This function will detect home, temp and if the current working
## directory is not in one of those then continue on to detect the cwd
## too.
##
## TODO: I don't know if this will work for all possible issues with
## path normalisation (e.g. if a mount occurs against a symlink?)
dide_detect_mount <- function(home, temp, shares, username) {
  dat <- detect_mount()
  ret <- list()

  if (is.null(home)) {
    is_home <- string_starts_with(tolower(dat[, "remote"]),
                                  "\\\\fi--san02\\homes")
    if (sum(is_home) == 1L) {
      ret$home <- path_mapping("home", dat[is_home, "local"],
                               dat[is_home, "remote"], "Q:")
    }
  } else {
    if (inherits(home, "path_mapping")) {
      ret$home <- home
    } else if (!identical(home, FALSE)) {
      ret$home <- path_mapping("home", home, dide_home("", username), "Q:")
    }
  }

  if (is.null(temp)) {
    is_temp <- string_starts_with(tolower(dat[, "remote"]),
                                  "\\\\fi--didef2\\tmp")
    if (sum(is_temp) == 1L) {
      ret$temp <- path_mapping("temp", dat[is_temp, "local"],
                               dide_temp(""), "T:")
    }
  } else {
    if (inherits(temp, "path_mapping")) {
      ret$temp <- temp
    } else if (!identical(temp, FALSE)) {
      ret$temp <- path_mapping("temp", temp, dide_temp(""), "T:")
    }
  }

  if (!is.null(shares)) {
    if (inherits(shares, "path_mapping")) {
      ret <- c(ret, setNames(list(shares), shares$name))
    } else if (is.list(shares)) {
      if (!all(vlapply(shares, inherits, "path_mapping"))) {
        stop("All elements of 'shares' must be a path_mapping")
      }
      ret <- c(ret, shares)
    } else {
      stop("Invalid input for 'shares'")
    }
  }

  mapped <- vcapply(ret, "[[", "path_local")

  remote <- vcapply(ret, "[[", "drive_remote", USE.NAMES=FALSE)
  dups <- unique(remote[duplicated(remote)])
  if (length(dups) > 0L) {
    stop("Duplicate remote drive names: ", paste(dups, collapse=", "))
  }

  ## TODO: The tolower needs to be done in a platform dependent way I
  ## think.  For now assume that Linux users don't store multiple
  ## relevant paths that differ in case.
  wd <- tolower(getwd())
  ok <- vlapply(tolower(mapped), string_starts_with, x=tolower(wd))
  if (!any(ok)) {
    i <- (nzchar(dat[, "local"]) &
          vlapply(tolower(dat[, "local"]), string_starts_with, x=tolower(wd)))
    if (sum(i) == 1L) {
      workdir <- path_mapping("workdir", dat[i, "local"],
                              dat[i, "remote"], available_drive(ret))
      ret <- c(ret, list(workdir=workdir))
    } else if (sum(i) > 1L) {
      ## Could take the *longest* here?
      warning("Having trouble determining the working directory mount point")
    }
  }

  ret
}

available_drive <- function(shares) {
  used <- toupper(substr(vcapply(shares, "[[", "drive_remote"), 1, 1))
  paste0(setdiff(LETTERS[22:26], used)[[1L]], ":")
}

## TODO: This will eventually be configurable, but for now is assumed
## in a few places -- search for R_VERSION (all caps).
R_VERSION <- numeric_version("3.2.4")
R_BITS <- 64L
R_PLATFORM <- if (R_BITS == 64L) "x86_64-w64-mingw32" else "i386-w64-mingw32"

## TODO: document how updates happen as there's some manual
## downloading and installation of rtools.
rtools_versions <- function(r_version, path=NULL) {
  r_version_2 <- as.character(r_version[1, 1:2])
  ret <- switch(r_version_2,
                "3.2"=list(path="Rtools33", gcc="4.6.3"),
                "3.3"=list(path="Rtools33", gcc="4.6.3"),
                stop("Get Rich to upgrade Rtools"))
  ret$path <- windows_path(file_path(path, "Rtools", ret$path))
  ret
}

rtools_info <- function(config) {
  tmpdrive <- NULL
  for (s in config$shares) {
    if (grepl("//fi--didef2/tmp/?", tolower(unname(s$path_remote)))) {
      tmpdrive <- s$drive_remote
      break
    }
  }
  if (is.null(tmpdrive)) {
    tmpdrive <- "//fi--didef2/tmp"
  }
  rtools_versions(R_VERSION, tmpdrive)
}

needs_rtools <- function(rtools, config, context) {
  rtools_pkgs <- c("rstan", "odin")
  isTRUE(unname(rtools)) || any(rtools_pkgs %in% context$packages)
}
