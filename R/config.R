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
##'   most useful with the \code{GeneralNodes} template.
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
##' @section Workers and rrq:
##'
##' The options \code{use_workers} and \code{use_rrq} interact, share
##' some functionality, but are quite different.
##'
##' With \code{use_workers}, jobs are never submitted when you run
##' \code{enqueue} or one of the bulk submission commands in
##' \code{queuer}.  Instead you submit workers using
##' \code{submit_workers} and then the submission commands push task
##' ids onto a Redis queue that the workers monitor.
##'
##' With \code{use_rrq}, \code{enqueue} etc still work as before, plus
##' you \emph{must} submit workers with \code{submit_workers}.  The
##' difference is that any job may access the \code{rrq_controller}
##' and push jobs onto a central pool of tasks.
##'
##' I'm not sure at this point if it makes any sense for the two
##' approaches to work together so this is disabled for now.  If you
##' think you have a use case please let me know.
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
##' @param cluster Name of the cluster to use; one of
##'   \code{\link{valid_clusters}()} or one of the aliases
##'   (small/little/dide/ide; big/mrc).
##'
##' @param shares Optional additional share mappings.  Can either be a
##'   single path mapping (as returned by \code{\link{path_mapping}}
##'   or a list of such calls.
##'
##' @param template A job template.  On fi--dideclusthn this can be
##'   "GeneralNodes" or "8Core", while on "fi--didemrchnb" this can be
##'   "GeneralNodes", "12Core", "16Core", "12and16Core", "20Core",
##'   "24Core" or "32Core". See the main cluster documentation if you
##'   tweak these parameters, as you may not have permission to use
##'   all templates (and if you use one that you don't have permission
##'   for the job will fail).  For training purposes there is also a
##'   "Training" template, but you will only need to use this when
##'   instructed to.
##'
##' @param cores The number of cores to request.  This is mostly
##'   useful when using the \code{GeneralNodes} template.  If
##'   specified, then we will request this many cores from the windows
##'   queuer.  If you request too many cores then your task will queue
##'   forever!  24 is the largest this should be on fi--dideclusthn
##'   and 64 on fi--didemrchnb (assuming you have access to those
##'   nodes).  If omitted then a single core is selected for the
##'   GeneralNodes template or the \emph{entire machine} for the other
##'   templates (unless modified by \code{wholenode}).
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
##' @param workdir The path to work in on the cluster, if running out of place.
##'
##' @param use_workers Submit jobs to an internal queue, and run them
##'   on a set of workers submitted separately?  If \code{TRUE}, then
##'   \code{enqueue} and the bulk submission commands no longer submit
##'   to the DIDE queue.  Instead they create an \emph{internal} queue
##'   that workers can poll.  After queuing tasks, use
##'   \code{submit_workers} to submit workers that will process these
##'   tasks, terminating when they are done.  You can use this
##'   approach to throttle the resources you need.
##'
##' @param use_rrq Use \code{rrq} to run a set of workers on the
##'   cluster.  This is an experimental option, and the interface here
##'   may change.  For now all this does is ensure a few additional
##'   packages are installed, and tweaks some environment variables in
##'   the generated batch files.  Actual rrq workers are submitted
##'   with the \code{submit_workers} method of the object.
##'
##' @param worker_timeout When using workers (via \code{use_workers}
##'   or \code{use_rrq}, the length of time (in seconds) that workers
##'   should be willing to set idle before exiting.  If set to zero
##'   then workers will be added to the queue, run jobs, and
##'   immediatly exit.  If greater than zero, then the workers will
##'   wait at least this many seconds after running the last task
##'   before quitting.  The number provoided can be `Inf`, in which
##'   case the worker will never exit (but be careful to clean the
##'   worker up in this case!).  The default is 600s (10 minutes)
##'   should be more than enough to get your jobs up and running.
##'   Once workers are established you can extend or reset the timeout
##'   by sending the \code{TIMEOUT_SET} message (proper documentation
##'   will come for this soon).
##'
##' @param rtools Make sure that rtools are installed (even if they
##'   aren't implicitly required by one of the required packages).  If
##'   \code{TRUE}, then network paths will be set up appropriately
##'   such that R on the cluster should find the appropriate version
##'   of rtools so that packages such as \code{rstan} and
##'   \code{Rcpp}'s inline functionality work correctly.
##'
##' @param r_version A string, or \code{numeric_version} object,
##'   describing the R version required.  Not all R versions are known
##'   to be supported, so this will check against a list of installed
##'   R versions for the cluster you are using (see
##'   \code{r_versions}).  If omitted then: if your R version matches
##'   a version on the cluster that will be used, or the oldest
##'   cluster version that is newer than yours, or the most recent
##'   cluster version.
##'
##' @param use_java Logical, indicating if the script is going to
##'   require Java, for example via the rJava package.
##'
##' @param java_home A string, optionally giving the path of a
##'   custom Java Runtime Environment, which will be used if
##'   the use_java logical is true. If left blank, then the
##'   default cluster Java Runtime Environment will be used.
##'
##'
##' @export
didehpc_config <- function(credentials = NULL, home = NULL, temp = NULL,
                           cluster = NULL,
                           shares = NULL, template = NULL, cores = NULL,
                           wholenode = NULL, parallel = NULL, hpctools = NULL,
                           workdir = NULL, use_workers = NULL, use_rrq = NULL,
                           worker_timeout = NULL, rtools = NULL,
                           r_version = NULL, use_java = NULL,
                           java_home = NULL) {
  defaults <- didehpc_config_defaults()
  given <- list(credentials = credentials,
                home = home,
                temp = temp,
                cluster = cluster,
                shares = shares,
                template = template,
                cores = cores,
                wholenode = wholenode,
                parallel = parallel,
                hpctools = hpctools,
                workdir = workdir,
                use_workers = use_workers,
                use_rrq = use_rrq,
                worker_timeout = worker_timeout,
                rtools = rtools,
                r_version = r_version,
                use_java = use_java,
                java_home = java_home)
  dat <- modify_list(defaults,
                     given[!vapply(given, is.null, logical(1))])
  ## NOTE: does *not* store (or request password)
  username <- get_credentials(dat$credentials, FALSE)$username
  if (is.null(dat$credentials)) {
    dat$credentials <- username
  }

  if (!is.null(dat$workdir)) {
    assert_scalar_character(dat$workdir, "workdir")
    if (!is_directory(dat$workdir)) {
      stop("workdir must be an existing directory")
    }
    workdir <- normalizePath(dat$workdir)
  }

  ## TODO: I'm not certain why this is the case.  Probably it can be
  ## got around with a couple of tweaks to rrq so that the same
  ## workers can be used for both approaches.
  if (isTRUE(use_workers) && isTRUE(use_rrq)) {
    stop("You can't specify both use_workers and use_rrq")
  }

  cluster <- cluster_name(dat$cluster)
  if (is.null(dat$template)) {
    dat$template <- valid_templates()[[cluster]][[1L]]
  }
  shares <- dide_detect_mount(dat$home, dat$temp, dat$shares,
                              workdir, username, cluster, FALSE)
  resource <- check_resources(cluster, dat$template, dat$cores,
                              dat$wholenode, dat$parallel)

  if (isTRUE(dat$hpctools)) {
    if (!has_hpctools()) {
      stop("HPC tools are requested but not found")
    }
  } else {
    dat$hpctools <- FALSE
  }
  dat$r_version <- select_r_version(dat$r_version)

  ## Set up the library path here
  browser()

  if (isTRUE(dat$use_java) && is.null(dat$java_home)) {
    dat$java_home <- ""
  }

  ret <- list(cluster = cluster,
              credentials = dat$credentials,
              username = username,
              template = dat$template,
              wholenode = dat$wholenode,
              hpctools = dat$hpctools,
              resource = resource,
              shares = shares,
              workdir = workdir,
              use_workers = dat$use_workers,
              use_rrq = dat$use_rrq,
              worker_timeout = dat$worker_timeout,
              rtools = dat$rtools,
              r_version = dat$r_version,
              use_java = dat$use_java,
              java_home = dat$java_home)

  class(ret) <- "didehpc_config"
  ret
}

##' @param ... arguments to \code{didehpc_config}
##' @export
##' @rdname didehpc_config
didehpc_config_global <- function(...) {
  opts <- list(...)
  if (length(opts) > 0L) {
    nms <- names(opts)
    if (is.null(nms) || any(nms == "")) {
      stop("All options must be named")
    }
    extra <- setdiff(nms, names(formals(didehpc_config)))
    if (length(extra)) {
      stop("Unknown options: ", paste(extra, collapse = ", "))
    }
    names(opts) <- paste0("didehpc.", nms)
    oo <- options(opts)
    on.exit(options(oo))
    if (!all(vlapply(opts, is.null))) {
      ## check that we're ok, if we actually set anything.
      tmp <- didehpc_config()
    }
    on.exit()
    invisible(oo)
  } else {
    invisible(list())
  }
}

didehpc_config_defaults <- function() {
  defaults <- list(
    cluster        = getOption("didehpc.cluster",        cluster_name(NULL)),
    credentials    = getOption("didehpc.credentials",    NULL),
    home           = getOption("didehpc.home",           NULL),
    temp           = getOption("didehpc.temp",           NULL),
    shares         = getOption("didehpc.shares",         NULL),
    template       = getOption("didehpc.template",       NULL),
    cores          = getOption("didehpc.cores",          NULL),
    wholenode      = getOption("didehpc.wholenode",      NULL),
    parallel       = getOption("didehpc.parallel",       NULL),
    workdir        = getOption("didehpc.workdir",        NULL),
    use_workers    = getOption("didehpc.use_workers",    FALSE),
    use_rrq        = getOption("didehpc.use_rrq",        FALSE),
    worker_timeout = getOption("didehpc.worker_timeout", 600),
    rtools         = getOption("didehpc.rtools",         TRUE),
    hpctools       = getOption("didehpc.hpctools",       FALSE),
    r_version      = getOption("didehpc.r_version",      NULL),
    use_java       = getOption("didehpc.use_java",       FALSE),
    java_home      = getOption("didehpc.java_home",      NULL))


  if (is.null(defaults$credentials)) {
    username <- getOption("didehpc.username", NULL)
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
print.didehpc_config <- function(x, ...) {
  cat("<didehpc_config>\n")
  expand <- c("numeric_version", "path_mapping")
  for (i in seq_along(x)) {
    el <- x[[i]]
    if (is.atomic(el) || inherits(el, expand)) {
      cat(sprintf(" - %s: %s\n", names(x)[[i]], as.character(el)))
    } else if (is.list(el)) {
      cat(sprintf(" - %s:\n", names(x)[[i]]))
      cat(paste(sprintf("    - %s: %s\n", names(el),
                        vcapply(el, as.character)), collapse = ""))
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

cluster_name <- function(name) {
  if (is.null(name)) {
    name <- valid_clusters()[[1L]]
  } else {
    assert_scalar_character(name)
    if (!(name %in% valid_clusters())) {
      alias <- list(
        "fi--dideclusthn" = c("small", "little", "dide", "ide", "dideclusthn"),
        "fi--didemrchnb" = c("big", "mrc", "didemrchnb"))
      alias <-
        setNames(rep(names(alias), lengths(alias)), unlist(alias, FALSE, FALSE))
      name <- alias[[match_value(tolower(name), names(alias), "name")]]
    }
  }
  name
}

valid_templates <- function() {
  list("fi--dideclusthn" = c("GeneralNodes", "8Core", "Training"),
       "fi--didemrchnb" = c("GeneralNodes", "12Core", "12and16Core", "16Core",
                            "20Core", "24Core", "32Core"))
}

check_resources <- function(cluster, template, cores, wholenode, parallel) {
  template <- match_value(template, valid_templates()[[cluster]])
  general <- template %in% c("GeneralNodes", "Training")

  if (!is.null(cores)) {
    if (isTRUE(wholenode)) {
      stop("Cannot specify both wholenode and cores")
    }
    ## assert_scalar_integer(cores)
    max_cores <- if (cluster == "fi--didemrchnb") 64 else 24
    if (cores > max_cores) {
      stop(sprintf("Maximum number of cores for %s is %d", cluster, max_cores))
    }
    parallel <- parallel %||% (cores > 1) # be careful of precendence
    ret <- list(parallel = parallel, count = cores, type = "Cores")
  } else if (!general || isTRUE(wholenode)) {
    ret <- list(parallel = parallel %||% TRUE, count = 1L, type = "Nodes")
  } else {
    ret <- list(parallel = parallel %||% FALSE, count = 1L, type = "Cores")
  }
  ret
}

## This function will detect home, temp and if the current working
## directory is not in one of those then continue on to detect the cwd
## too.
##
## TODO: I don't know if this will work for all possible issues with
## path normalisation (e.g. if a mount occurs against a symlink?)
dide_detect_mount <- function(home, temp, shares, workdir, username, cluster,
                              silent = FALSE) {
  dat <- detect_mount()
  ret <- list()

  if (is.null(home)) {
    ## Try to detect where home is currently mounted because Oliver
    ## keeps his on O.
    re <- "^\\\\\\\\(qdrive|fi--san0[23])(\\.dide\\.ic\\.ac\\.uk)?\\\\homes\\\\"
    is_home <- grepl(re, tolower(dat[, "remote"]))
    if (sum(is_home) == 1L) {
      ret$home <- path_mapping("home", dat[is_home, "local"],
                               dat[is_home, "remote"], "Q:")
    } else if (sum(is_home) > 1L) {
      stop(sprintf(
        "I am confused about your home directory; there are %d choices:\n%s",
        sum(is_home),
        paste(sprintf("   - %s => %s",
                      dat[is_home, "local"],
                      dat[is_home, "remote"]), collapse = "\n")))
    } else {
      ## For now, require that home is given otherwise there are a few
      ## things that might not work.  This might actually be OK but
      ## needs testing I think.  Test this with passing FALSE through
      ## and see what I can make break
      stop("I can't find your home directory!  Please mount it")
    }
  } else {
    if (inherits(home, "path_mapping")) {
      ret$home <- home
    } else if (!identical(home, FALSE)) {
      ret$home <- path_mapping("home", home, dide_home("", username), "Q:")
    }
  }

  if (grepl("^//fi--san02(/.dide/.ic/.ac/.uk)?/homes",
            ret$home$path_remote)) {
    home_path_new <- sub("//fi--san02", "//fi--san03",
                         ret$home$path_remote, fixed = TRUE)
    msg <-
      c("Your home drive uses the old 'fi--san02' mapping:",
        paste0("    ", ret$home$path_remote),
        sprintf("You'll need to remap this share (locally mounted at '%s')",
                ret$home$path_local),
        "to point at the new network location on 'fi--san03'",
        paste0("    ", home_path_new))
    warning(paste(msg, collapse = "\n"), immediate. = TRUE, call. = FALSE)
    ret$home$path_remote <- home_path_new
  }

  if (is.null(temp)) {
    is_temp <- string_starts_with(tolower(dat[, "remote"]),
                                  "\\\\fi--didef3\\tmp")
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

  remote <- vcapply(ret, "[[", "drive_remote", USE.NAMES = FALSE)
  dups <- unique(remote[duplicated(remote)])
  if (length(dups) > 0L) {
    stop("Duplicate remote drive names: ", paste(dups, collapse = ", "))
  }

  ## TODO: The tolower needs to be done in a platform dependent way I
  ## think.  For now assume that Linux users don't store multiple
  ## relevant paths that differ in case.
  if (is.null(workdir)) {
    workdir <- getwd()
  }
  ## TODO: this tolower should be windows only
  workdir <- tolower(workdir)

  ok <- vlapply(tolower(mapped), string_starts_with, x = workdir)
  if (!any(ok)) {
    i <- (nzchar(dat[, "local"]) &
          vlapply(tolower(dat[, "local"]), string_starts_with, x = workdir))
    if (sum(i) == 1L) {
      ## On windows this should go elsewhere.
      drive <- if (is_windows()) dat[i, "local"] else available_drive(ret)
      workdir_map <- path_mapping("workdir", dat[i, "local"],
                                  dat[i, "remote"], drive)
      ret <- c(ret, list(workdir = workdir_map))
    } else if (sum(i) > 1L) {
      ## Could take the *longest* here?
      warning("Having trouble determining the working directory mount point")
    } else { # sum(i) == 0
      ## NOTE: This needs to be checked later when firing up the
      ## queue, but I believe that it is.
      if (!silent) {
        message(sprintf("Running out of place: %s is not on a network share",
                        workdir))
      }
    }
  }

  if (cluster == "fi--didemrchnb") {
    for (i in seq_along(ret)) {
      ret[[i]]$path_remote <-
               sub("^//(fi--didenas[1345])/", "//\\1-app/",
                   ret[[i]]$path_remote)
    }
  }
  ret
}

available_drive <- function(shares) {
  used <- toupper(substr(vcapply(shares, "[[", "drive_remote"), 1, 1))
  paste0(setdiff(LETTERS[22:26], used)[[1L]], ":")
}

R_BITS <- 64

## TODO: document how updates happen as there's some manual
## downloading and installation of rtools.
rtools_versions <- function(r_version, path = NULL) {
  r_version_2 <- as.character(r_version[1, 1:2])
  if (r_version < "4.0.0") {
    mingw <- sprintf("mingw_%d", R_BITS)
  } else {
    mingw <- sprintf("mingw%d", R_BITS)
  }

  ret <- switch(r_version_2,
    "3.3" = list(path = "Rtools35", gcc = mingw, make = ""),
    "3.4" = list(path = "Rtools35", gcc = mingw, make = ""),
    "3.5" = list(path = "Rtools35", gcc = mingw, make = ""),
    "3.6" = list(path = "Rtools35", gcc = mingw, make = ""),
    "4.0" = list(path = "Rtools40", gcc = mingw, make = "usr"),
    stop(sprintf("No RTools version found for R %s", r_version)))

  ret$binpref <-
    unix_path(file.path(path, "Rtools", ret$path, mingw, "bin"))

  ret$rtools_root <- windows_path(file_path(path, "Rtools", ret$path))
  ret$gcc_path <- windows_path(file_path(ret$rtools_root, ret$gcc, "bin"))
  ret$make_path <- windows_path(file_path(ret$rtools_root, ret$make, "bin"))

  ret$path <- NULL
  ret
}

rtools_info <- function(config) {
  tmpdrive <- NULL

  for (s in config$shares) {
    if (grepl("//fi--didef3/tmp/?", tolower(unname(s$path_remote)))) {
      tmpdrive <- s$drive_remote
      break
    }
  }
  if (is.null(tmpdrive)) {
    tmpdrive <- "//fi--didef3/tmp"
  }
  rtools_versions(config$r_version, tmpdrive)
}


redis_host <- function(cluster) {
  switch(cluster,
         "fi--didemrchnb" = "12.0.0.1",
         "fi--dideclusthn" = "11.0.0.1",
         stop(sprintf("No redis host for cluster '%s'", cluster)))
}


## NOTE: Only some versions of R are supported by context; at present
## we require >= 3.2.2.
##
## NOTE: Practically supporting old versions (currently 3.3.x and
## below) requires work in at least provisionr to get all the packages
## built.  See didehpc issue #54
r_versions <- function() {
  if (is.null(cache$r_versions)) {
    r <- httr::GET("https://mrcdata.dide.ic.ac.uk/hpc/api/v1/cluster_software/")
    v <- from_json(httr::content(r, as = "text", encoding = "UTF-8"))
    cache$r_versions <- vcapply(
      v$software[vlapply(v$software, function(x) x$name == 'R')], "[[", "version")

  }
  numeric_version(cache$r_versions)
}

select_r_version <- function(r_version, ours = getRversion()) {
  if (is.null(r_version)) {
    valid <- r_versions()
    if (ours %in% valid) {
      r_version <- numeric_version(ours)
    } else {
      i <- valid > ours
      j <- if (any(i)) which(i)[[1L]] else length(valid)
      r_version <- valid[[j]]
    }
  } else {
    if (is.character(r_version)) {
      r_version <- numeric_version(r_version)
    }
    if (!(r_version %in% r_versions())) {
      stop("Unsupported R version: ", as.character(r_version))
    }
  }
  r_version
}

## This is a really horrific function and it's likely to be a source
## of misbehaviour.  Probably a much better way would be to get people
## to use a different format and generate the XML from it.
check_linux_shares <- function(username, shares) {
  ## This bit _always_ holds, we hope:
  mount_root <- sprintf("/homes/%s/dide", username)
  shares$home$drive_remote <- file.path(mount_root, "home", fsep = "/")

  dide_home_local <- shares$home$path_local
  pam_conf <- file.path(dide_home_local, ".pam_mount.conf.xml")

  re_fqn <- "\\.dide\\.(ic|imperial)\\.ac.\\uk"
  has_conf <- file.exists(pam_conf)

  if (has_conf) {
    pam_xml <- xml2::read_xml(pam_conf)
    volumes <- xml2::xml_attrs(xml2::xml_find_all(pam_xml, "/pam_mount/volume"))

    # Remove temp drive if it's found in pam.xml, as we always add it anyway.

    tmp_mount <- paste0("/homes/", username, "/dide/tmp")
    vol_seq <- seq(from = length(volumes), to = 1, by = -1)
    for (vol_no in vol_seq) {
      if (volumes[[vol_no]][['mountpoint']] == tmp_mount)
        volumes[[vol_no]] <- NULL
    }

    v_mountpoint <- vcapply(volumes, "[[", "mountpoint")
    v_server <- paste0("//", sub(re_fqn, "", vcapply(volumes, "[[", "server")))
    v_path <- vcapply(volumes, "[[", "path")
    v_full <- file.path(v_server, v_path, fsep = "/")

    for (i in which(names(shares) != "home")) {
      s <- shares[[i]]
      j <- match(sub(re_fqn, "", s$path_remote), v_full)
      if (is.na(j)) {
        s$drive_remote <- NULL
      } else {
        s$drive_remote <- v_mountpoint[[j]]
      }
      shares[[i]] <- s
    }
  } else {
    v_mountpoint <- "home"
    for (i in which(names(shares) != "home")) {
      shares[[i]]$drive_remote <- NULL
    }
    volumes <- NULL
  }

  ## This is all super ugly, partly because xml2 doesn't yet support
  ## writing xml nicely (and doesn't pretty print what it does write).
  msg <- vlapply(shares, function(x) is.null(x$drive_remote))
  if (any(msg)) {
    base <- cbind(options = "vers=2.1,nodev,nosuid", user = "*",
                  fstype = "cifs")
    remote <- vcapply(shares[msg], "[[", "path_remote")
    re_remote <- "^//(?<server>[^/]*)/(?<path>.*)/*"
    dat <- rematch::re_match(re_remote, remote)
    dat <- dat[, c("server", "path"), drop = FALSE]
    dat[, "server"] <- paste0(dat[, "server"], ".dide.ic.ac.uk")
    base <- base[rep(1, nrow(dat)), , drop = FALSE]
    dat <- cbind(base, dat,
                 mountpoint = file.path(mount_root, basename(remote)))

    for (i in seq_len(nrow(dat))) {
      shares[[which(msg)[i]]]$drive_remote <- dat[i, "mountpoint"]
    }

    if (has_conf) {
      tmp <- c(v_mountpoint, dat[, "mountpoint"])
      nok <- duplicated(tmp)
      if (any(nok)) {
        stop("Avoiding creating duplicate mountpoints")
      }
      message(sprintf("Adding %d entries into existing pam file %s",
                      nrow(dat), pam_conf))
      backup(pam_conf)
    } else {
      message(sprintf("Writing %d entries into new pam file %s",
                      nrow(dat), pam_conf))
    }
    dat <- c(volumes, lapply(seq_len(nrow(dat)), function(i) dat[i, ]))

    ## Then, format:
    opts <- vcapply(dat, function(x)
      paste(names(x), dquote(unname(x)), sep = "=", collapse = " "))
    volumes <- sprintf("<volume %s />", opts)
    opts <- paste(colnames(dat), sprintf('"%s"', unname(dat)),
                  sep = "=", collapse = " ")
    xml <- c('<?xml version="1.0" encoding="utf-8" ?>',
             '<pam_mount>',
             paste0("  ", volumes),
             '</pam_mount>')
    writeLines(xml, pam_conf)
  }
  shares
}
