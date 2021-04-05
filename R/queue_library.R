queue_library <- R6::R6Class(
  "queue_library",
  public = list(
    path_library = NULL,
    path_conan = NULL,
    shares = NULL,
    template = NULL,
    cluster = NULL,
    client = NULL,

    initialize = function(path_library, path_conan, shares,
                          template, cluster, client) {
      self$path_library <- path_library
      self$path_conan <- path_conan
      self$shares <- shares
      self$template <- template
      self$cluster <- cluster
      self$client <- client
    },

    check = function(packages) {
      conan::conan_check(packages, self$path_library)
    },

    write_batch = function(packages, repos = NULL, policy = "upgrade",
                           dryrun = FALSE) {
      id <- ids::random_id()
      path_script <- file.path(self$path_conan, "bin", id)
      path_batch <- file.path(self$path_conan, "batch", paste0(id, ".bat"))
      path_log <- file.path(self$path_conan, "log", id)
      dat <- list(conan_id = id)

      conan::conan(path_script, packages, repos = repos, policy = policy,
                   dryrun = dryrun)

      dir.create(dirname(path_batch), FALSE, TRUE)
      dir.create(dirname(path_log), FALSE, TRUE)

      writeLines(whisker::whisker.render(self$template, dat), path_batch)
      list(id = id, batch = path_batch, script = path_script, log = path_log)
    },

    provision = function(packages, repos = NULL, policy = "upgrade",
                         dryrun = FALSE) {
      self$client$login()
      dat <- self$write_batch(packages, repos, policy, dryrun)

      ## Make sure that the path exists; it's possible that this is
      ## not needed - check in conan/pkgdepends
      dir.create(self$path_library, FALSE, TRUE)
      ## Also may not be needed? Check.
      dir.create(file.path(self$path_conan, "cache"), FALSE, TRUE)

      path <- prepare_path(dat$batch, self$shares)
      path_batch <- windows_path(file.path(path$path_remote, path$rel))

      name <- paste0("conan:", dat$id)
      job_template <- queue_template(cluster)
      dide_id <- self$client$submit(path_batch, name, job_template,
                                    self$cluster)
      provision_watch(dide_id, cluster, dat$log, self$client)
    }
  ))


queue_template <- function(cluster) {
  if (cluster == "fi--didemrchnb") "BuildNodes" else "GeneralNodes"
}


provision_watch <- function(dide_id, cluster, path_log, client, force = FALSE,
                            poll = 1) {
  fmt <- "[:spin] :elapsed :status"
  p <- progress::progress_bar$new(fmt, NA, show_after = 0, force = force)
  p$tick(0, list(status = "..."))

  status_job <- throttle(client$status_job, poll)

  log_prev <- NULL
  repeat {
    status <- status_job(dide_id, cluster)
    log <- readlines_if_exists(path_log, warn = FALSE)

    if (length(log) > length(log_prev)) {
      clear_progress_bar(p)
      message(paste(new_log(log, log_prev), collapse = "\n"))
      log_prev <- log
    }

    if (status %in% c("COMPLETE", "ERROR")) {
      p$terminate()
      break
    } else {
      p$tick(tokens = list(status = status))
    }
  }

  if (status == "ERROR") {
    stop("Installation failed ")
  }
}


context_packages <- function(context, need_rrq = FALSE) {
  list(packages = c("context",
                    if (need_rrq) "rrq",
                    context$packages$attached,
                    context$packages$loaded,
                    context$package_sources$packages),
       repos = c(context$package_sources$repos,
                 didehpc = "https://mrc-ide.github.io/didehpc-pkgs"))
}
