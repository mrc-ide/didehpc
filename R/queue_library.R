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
                         dryrun = FALSE,
                         show_progress = TRUE, show_log = TRUE) {
      client <- self$client
      cluster <- self$cluster
      client$login()
      dat <- self$write_batch(packages, repos, policy, dryrun)

      path <- prepare_path(dat$batch, self$shares)
      path_batch <- windows_path(file.path(path$path_remote, path$rel))

      name <- paste0("conan:", dat$id)
      job_template <- queue_template(self$cluster)
      dide_id <- client$submit(path_batch, name, job_template, cluster)

      conan::conan_watch(
        function() client$status_job(dide_id, cluster),
        function() readlines_if_exists(dat$log, warn = FALSE),
        show_progress, show_log)
    }
  ))


queue_template <- function(cluster) {
  if (cluster == "fi--didemrchnb") "BuildQueue" else "GeneralNodes"
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
