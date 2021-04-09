queue_library <- R6::R6Class(
  "queue_library",
  public = list(
    data = NULL,
    cluster = NULL,
    client = NULL,

    ## path_library: the path to the library as reachable from this node
    ## path_conan: the path to conan's root...
    ##   local: ...as reachable from this node
    ##   remote: ...via a UNC path
    initialize = function(data, cluster, client) {
      self$data <- data
      self$cluster <- cluster
      self$client <- client
    },

    check = function(packages) {
      conan::conan_check(packages, self$data$paths$local$lib)
    },

    write_batch = function(packages, repos = NULL, policy = "upgrade",
                           dryrun = FALSE) {
      id <- ids::random_id()
      path_conan <- self$data$paths$local$conan
      batch <- file.path("batch", paste0(id, ".bat"))
      batch_local <- file.path(path_conan, batch)
      ret <- list(
        id = id,
        name = paste0("conan:", id),
        batch = windows_path(file.path(self$data$paths$remote$conan, batch)),
        script = file.path(path_conan, "bin", id),
        log = file.path(path_conan, "log", id))
      dir.create(dirname(batch_local), FALSE, TRUE)
      dir.create(dirname(ret$log), FALSE, TRUE)

      conan::conan(ret$script, packages, repos = repos, policy = policy,
                   dryrun = dryrun)
      writeLines(glue_whisker(self$data$templates$conan, list(conan_id = id)),
                 batch_local)
      ret
    },

    provision = function(packages, repos = NULL, policy = "upgrade",
                         dryrun = FALSE, show_progress = TRUE,
                         show_log = TRUE, poll = 1) {
      cluster <- self$cluster
      client <- self$client
      client$login()
      dat <- self$write_batch(packages, repos, policy, dryrun)

      job_template <- queue_template(cluster)
      dide_id <- client$submit(dat$batch, dat$name, job_template, cluster)

      conan::conan_watch(
        function() client$status_job(dide_id, cluster),
        function() readlines_if_exists(dat$log, warn = FALSE),
        show_progress, show_log, poll = poll)
    }
  ))


queue_template <- function(cluster) {
  if (cluster == "fi--didemrchnb") {
    "BuildQueue"
  } else {
    "GeneralNodes"
  }
}


provision_policy <- function(policy, name = deparse(substitute(name))) {
  if (is.logical(policy)) {
    policy <- if (policy) "lazy" else "later"
  }
  match_value(policy, c("lazy", "upgrade", "later", "fake"), name)
}
