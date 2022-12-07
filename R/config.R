##' Collects configuration information.  Unfortunately there's a
##' fairly complicated process of working out what goes where so
##' documentation coming later.
##'
##' @section Resources and parallel computing:
##'
##' If you need more than one core per task (i.e., you want the each
##'   task to do some parallel processing *in addition* to the
##'   parallelism between tasks) you can do that through the
##'   configuration options here.
##'
##' The `template` option chooses among templates defined on the
##'   cluster.  If you select one of these then we will reserve an
##'   entire node *unless* you also specify `cores`.
##'   Alternatively if `wholenode` is specified this overrides
##'   the logic here.
##'
##' If you specify `cores`, the HPC will queue your job until an
##'   appropriate number of cores appears for the selected template.
##'   This can leave your job queuing forever (e.g., selecting 20 cores
##'   on a 16Core template) so be careful.  The `cores` option is
##'   most useful with the `GeneralNodes` template.
##'
##' In either case, if more than 1 core is implied (either by using
##'   any template other than `GeneralNodes` or by specifying a
##'   `cores` value greater than 1) on startup, a `parallel`
##'   cluster will be started, using `parallel::makePSOCKcluster`
##'   and this will be registered as the default cluster.  The nodes
##'   will all have the appropriate context loaded and you can
##'   immediately use them with `parallel::clusterApply` and
##'   related functions by passing `NULL` as the first argument.
##'   The cluster will be shut down politely on exit, and logs will be
##'   output to the "workers" directory below your context root.
##'
##' @section Workers and rrq:
##'
##' The options `use_workers` and `use_rrq` interact, share
##' some functionality, but are quite different.
##'
##' With `use_workers`, jobs are never submitted when you run
##' `enqueue` or one of the bulk submission commands in
##' `queuer`.  Instead you submit workers using
##' `submit_workers` and then the submission commands push task
##' ids onto a Redis queue that the workers monitor.
##'
##' With `use_rrq`, `enqueue` etc still work as before, plus
##' you *must* submit workers with `submit_workers`.  The
##' difference is that any job may access the `rrq_controller`
##' and push jobs onto a central pool of tasks.
##'
##' I'm not sure at this point if it makes any sense for the two
##' approaches to work together so this is disabled for now.  If you
##' think you have a use case please let me know.
##'
##' @title Configuration
##'
##' @param credentials Either a list with elements username, password,
##'   or a path to a file containing lines `username=<username>`
##'   and `password=<password>` or your username (in which case
##'   you will be prompted graphically for your password).
##'
##' @param home Path to network home directory, on local system
##'
##' @param temp Path to network temp directory, on local system
##'
##' @param cluster Name of the cluster to use; one of
##'   [didehpc::valid_clusters()] or one of the aliases
##'   (small/little/dide/ide; big/mrc).
##'
##' @param shares Optional additional share mappings.  Can either be a
##'   single path mapping (as returned by [didehpc::path_mapping()]
##'   or a list of such calls.
##'
##' @param template A job template.  On fi--dideclusthn this can be
##'   "GeneralNodes" or "8Core". On "fi--didemrchnb" this can be
##'   "GeneralNodes", "12Core", "16Core", "12and16Core", "20Core",
##'   "24Core", "32Core", or "MEM1024" (for nodes with 1Tb of RAM; we have
##'   three - two of which have 32 cores, and the other is the AMD epyc with
##'   64). On the new "wpia-hpc-hn" cluster, you should
##'   currently use "AllNodes". See the main cluster documentation if you
##'   tweak these parameters, as you may not have permission to use
##'   all templates (and if you use one that you don't have permission
##'   for the job will fail).  For training purposes there is also a
##'   "Training" template, but you will only need to use this when
##'   instructed to.
##'
##' @param cores The number of cores to request.  This is mostly
##'   useful when using the `GeneralNodes` template.  If
##'   specified, then we will request this many cores from the windows
##'   queuer.  If you request too many cores then your task will queue
##'   forever!  24 is the largest this should be on fi--dideclusthn,
##'   64 on fi--didemrchnb and 32 on wpia-hpc-hn (assuming you have access to those
##'   nodes).  If omitted then a single core is selected for the
##'   GeneralNodes template or the *entire machine* for the other
##'   templates (unless modified by `wholenode`).
##'
##' @param wholenode Request the whole node?  This will default to
##'   `TRUE` if any template other than `GeneralNodes` is
##'   selected.
##'
##' @param parallel Should we set up the parallel cluster?  Normally
##'   if more than one core is implied (via the `cores` argument,
##'   by picking a template other than `GeneralNodes` or by using
##'   `wholenode`) then a parallel cluster will be set up (see
##'   Details).  If `parallel` is set to `FALSE` then this
##'   will not occur.  This might be useful in cases where you want to
##'   manage your own job level parallelism (e.g. using OpenMP) or if
##'   you're just after the whole node for the memory).
##'
##' @param workdir The path to work in on the cluster, if running out of place.
##'
##' @param use_workers Submit jobs to an internal queue, and run them
##'   on a set of workers submitted separately?  If `TRUE`, then
##'   `enqueue` and the bulk submission commands no longer submit
##'   to the DIDE queue.  Instead they create an *internal* queue
##'   that workers can poll.  After queuing tasks, use
##'   `submit_workers` to submit workers that will process these
##'   tasks, terminating when they are done.  You can use this
##'   approach to throttle the resources you need.
##'
##' @param use_rrq Use `rrq` to run a set of workers on the
##'   cluster.  This is an experimental option, and the interface here
##'   may change.  For now all this does is ensure a few additional
##'   packages are installed, and tweaks some environment variables in
##'   the generated batch files.  Actual rrq workers are submitted
##'   with the `submit_workers` method of the object.
##'
##' @param worker_timeout When using workers (via `use_workers`
##'   or `use_rrq`, the length of time (in seconds) that workers
##'   should be willing to set idle before exiting.  If set to zero
##'   then workers will be added to the queue, run jobs, and
##'   immediately exit.  If greater than zero, then the workers will
##'   wait at least this many seconds after running the last task
##'   before quitting.  The number provided can be `Inf`, in which
##'   case the worker will never exit (but be careful to clean the
##'   worker up in this case!).  The default is 600s (10 minutes)
##'   should be more than enough to get your jobs up and running.
##'   Once workers are established you can extend or reset the timeout
##'   by sending the `TIMEOUT_SET` message (proper documentation
##'   will come for this soon).
##'
##' @param worker_resource Optionally, an object created by
##'   [didehpc::worker_resource()] which controls the resources used
##'   by workers where these are different to jobs directly submitted
##'   by `$enqueue()`. This is only meaningful if you are using
##'   `use_rrq = TRUE`.
##'
##' @param conan_bootstrap Logical, indicating if we should use the
##'   shared conan "bootstrap" library stored on the temporary
##'   directory. Setting this to `FALSE` will install all dependencies
##'   required to install packages first into a temporary location
##'   (this may take a few minutes) before installation. Generally
##'   leave this as-is.
##'
##' @param r_version A string, or `numeric_version` object, describing
##'   the R version required.  Not all R versions are known to be
##'   supported, so this will check against a list of installed R
##'   versions for the cluster you are using.  If omitted then: if
##'   your R version matches a version on the cluster that will be
##'   used, or the oldest cluster version that is newer than yours, or
##'   the most recent cluster version.
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
                           wholenode = NULL, parallel = NULL,
                           workdir = NULL, use_workers = NULL, use_rrq = NULL,
                           worker_timeout = NULL, worker_resource = NULL,
                           conan_bootstrap = NULL, r_version = NULL,
                           use_java = NULL, java_home = NULL) {
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
                workdir = workdir,
                use_workers = use_workers,
                use_rrq = use_rrq,
                worker_timeout = worker_timeout,
                conan_bootstrap = conan_bootstrap,
                r_version = r_version,
                use_java = use_java,
                java_home = java_home)
  dat <- modify_list(defaults,
                     given[!vapply(given, is.null, logical(1))])

  credentials <- dide_credentials(dat$credentials, FALSE)

  if (!is.null(dat$workdir)) {
    assert_scalar_character(dat$workdir, "workdir")
    if (!is_directory(dat$workdir)) {
      stop("workdir must be an existing directory")
    }
    workdir <- normalizePath(dat$workdir)
  }

  cluster <- cluster_name(dat$cluster)
  if (is.null(dat$template)) {
    dat$template <- valid_templates()[[cluster]][[1L]]
  }
  mounts <- detect_mount()
  remap_nas <- cluster %in% c("fi--didemrchnb", "wpia-hpc-hn", "wpia-hn")
  shares <- dide_detect_mount(mounts, dat$shares, dat$home, dat$temp,
                              workdir, credentials$username, remap_nas, cluster)
  resource <- check_resources(cluster, dat$template, dat$cores,
                              dat$wholenode, dat$parallel)

  dat$r_version <- select_r_version(dat$r_version)

  if (isTRUE(dat$use_java) && is.null(dat$java_home)) {
    dat$java_home <- ""
  }

  ret <- list(cluster = cluster,
              credentials = credentials,
              username = credentials$username,
              wholenode = dat$wholenode,
              resource = resource,
              shares = shares,
              workdir = workdir,
              use_workers = dat$use_workers,
              use_rrq = dat$use_rrq,
              worker_timeout = dat$worker_timeout,
              conan_bootstrap = dat$conan_bootstrap,
              r_version = dat$r_version,
              use_java = dat$use_java,
              java_home = dat$java_home,
              ## derived values
              redis_host = paste0(cluster, ".dide.ic.ac.uk"))

  if (!is.null(worker_resource)) {
    if (!ret$use_rrq) {
      stop("'worker_resource' provided but 'use_rrq' is FALSE")
    }
    ret$worker_resource <-
      check_worker_resource(worker_resource, cluster, dat$template,
                            dat$cores, dat$wholenode, dat$parallel)
  }

  class(ret) <- "didehpc_config"
  ret
}


##' Specify resources for worker processes. If given, the values here
##' will override those in [didehpc::didehpc_config()]. See
##' `vignette("workers")` for more details.
##'
##' @title Specify worker resources
##'
##' @inheritParams didehpc_config
##'
##' @return A list with class `worker_resource` which can be passed
##'   into [`didehpc_config`]
##'
##' @export
worker_resource <- function(template = NULL, cores = NULL,
                            wholenode = NULL, parallel = NULL) {
  ret <- list(template = template, cores = cores, wholenode = wholenode,
              parallel = parallel)
  class(ret) <- "worker_resource"
  ret
}


as_didehpc_config <- function(config) {
  if (!inherits(config, "didehpc_config")) {
    if (is.list(config)) {
      config <- do.call("didehpc_config", config)
    } else {
      stop("Expected a didehpc_config for 'config'")
    }
  }
  config
}


##' @param ... arguments to `didehpc_config`
##'
##' @param check Logical, indicating if we should check that the
##'   configuration object can be created
##'
##' @export
##' @rdname didehpc_config
didehpc_config_global <- function(..., check = TRUE) {
  opts <- list(...)
  if (length(opts) > 0L) {
    nms <- names(opts)
    if (is.null(nms) || any(nms == "")) {
      stop("All options must be named")
    }
    extra <- setdiff(nms, names(didehpc_config_defaults()))
    if (length(extra)) {
      stop("Unknown options: ", paste(extra, collapse = ", "))
    }
    names(opts) <- paste0("didehpc.", nms)
    oo <- options(opts)
    on.exit(options(oo))
    if (check && !all(vlapply(opts, is.null))) {
      ## check that we're ok, if we actually set anything.
      didehpc_config()
    }
    on.exit()
    invisible(oo)
  } else {
    invisible(list())
  }
}


didehpc_config_defaults <- function() {
  defaults <- list(
    cluster         = getOption("didehpc.cluster",         cluster_name(NULL)),
    credentials     = getOption("didehpc.credentials",     NULL),
    home            = getOption("didehpc.home",            NULL),
    temp            = getOption("didehpc.temp",            NULL),
    shares          = getOption("didehpc.shares",          NULL),
    template        = getOption("didehpc.template",        NULL),
    cores           = getOption("didehpc.cores",           NULL),
    wholenode       = getOption("didehpc.wholenode",       NULL),
    parallel        = getOption("didehpc.parallel",        NULL),
    workdir         = getOption("didehpc.workdir",         NULL),
    use_workers     = getOption("didehpc.use_workers",     FALSE),
    use_rrq         = getOption("didehpc.use_rrq",         FALSE),
    worker_timeout  = getOption("didehpc.worker_timeout",  600),
    conan_bootstrap = getOption("didehpc.conan_bootstrap", TRUE),
    r_version       = getOption("didehpc.r_version",       NULL),
    use_java        = getOption("didehpc.use_java",        FALSE),
    java_home       = getOption("didehpc.java_home",       NULL))


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
  c("fi--dideclusthn", "fi--didemrchnb", "wpia-hpc-hn", "wpia-hn")
}


cluster_name <- function(name) {
  if (is.null(name)) {
    name <- valid_clusters()[[1L]]
  } else {
    assert_scalar_character(name)
    if (!(name %in% valid_clusters())) {
      alias <- list(
        "fi--dideclusthn" = c("small", "little", "dide", "ide", "dideclusthn"),
        "fi--didemrchnb" = c("big", "mrc", "didemrchnb"),
        "wpia-hpc-hn" = "covid")
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
                            "20Core", "24Core", "32Core", "MEM1024"),
       "wpia-hpc-hn" = "AllNodes",
       "wpia-hn" = "AllNodes")
}

didehpc_check_max_cores <- function(cluster, cores) {
  max_cores <- 
    switch(cluster,
         "fi--dideclusthn" = 24,
         "fi--didemrchnb" = 64,
         "wpia-hpc-hn" = 32,
         "wpia-hn" = 32,
         stop(sprintf("Invalid cluster '%s'", cluster)))
  if (cores > max_cores) {
    stop(sprintf("Maximum number of cores for %s is %d", cluster, max_cores))
  }
}

check_resources <- function(cluster, template, cores, wholenode, parallel) {
  template <- match_value(template, valid_templates()[[cluster]])
  general <- template %in% c("GeneralNodes", "Training")

  if (!is.null(cores)) {
    if (isTRUE(wholenode)) {
      stop("Cannot specify both wholenode and cores")
    }
    assert_scalar_integer(cores)
    didehpc_check_max_cores(cluster, cores)
    parallel <- parallel %||% (cores > 1) # be careful of precendence
    ret <- list(template = template, parallel = parallel,
                count = cores, type = "Cores")
  } else if (!general || isTRUE(wholenode)) {
    ret <- list(template = template, parallel = parallel %||% TRUE,
                count = 1L, type = "Nodes")
  } else {
    ret <- list(template = template, parallel = parallel %||% FALSE,
                count = 1L, type = "Cores")
  }
  ret
}


check_worker_resource <- function(worker_resource, cluster, template,
                                  cores, wholenode, parallel) {
  assert_is(worker_resource, "worker_resource")
  tryCatch(
    check_resources(cluster,
                    worker_resource$template %||% template,
                    worker_resource$cores %||% cores,
                    worker_resource$wholenode %||% wholenode,
                    worker_resource$parallel %||% parallel),
    error = function(e) stop("Invalid worker resource request: ", e$message,
                             call. = FALSE))
}

redis_host <- function(cluster) {
  switch(cluster,
         "wpia-hpc-hn" = "12.0.1.254",
         "fi--didemrchnb" = "12.0.0.1",
         "fi--dideclusthn" = "11.0.0.1",
         "wpia-hn" = "10.0.3.254",
         stop(sprintf("No redis host for cluster '%s'", cluster)))
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


## TODO: We might improve this because we *always* need the temp drive now
remote_drive_temp <- function(shares) {
  for (s in shares) {
    re <- "^\\\\\\\\fi--didef3(\\.dide\\.ic\\.ac\\.uk)?\\\\tmp"
    if (grepl(re, s$path_remote, ignore.case = TRUE)) {
      return(s$drive_remote)
    }
  }
  NULL
}
