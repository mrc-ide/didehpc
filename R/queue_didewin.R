##' Create a queue object
##'
##' Queues are R6 objects with many methods.  They need documenting still.
##' @title Create a queue object
##' @param context A context
##' @param config Optional dide configuration information.
##' @param login log in to the cluster on queue creation (recommended)
##' @export
queue_didewin <- function(context, config=didewin_config(), login=TRUE) {
  .R6_queue_didewin$new(context, config, login)
}

.R6_queue_didewin <- R6::R6Class(
  "queue_didewin",
  inherit=queuer:::.R6_queue_base,
  public=list(
    config=NULL,
    initialize=function(context, config, login) {
      super$initialize(context)
      self$config <- config

      dir.create(path_batch(context$root), FALSE, TRUE)
      dir.create(path_logs(context$root), FALSE, TRUE)
      ## Not sure about these two:
      ##   dir.create(path_dide_task_id(context$root), FALSE, TRUE)
      ##   dir.create(path_dide_cluster(context$root), FALSE, TRUE)
      ## -- instead, do this via submit()

      if (login) {
        self$login()
      }
    },

    login=function() {
      web_login(self$config)
    },
    set_cluster=function(cluster=NULL) {
      if (is.null(cluster)) {
        cluster <- setdiff(valid_clusters(), self$config$cluster)
      } else {
        cluster <- match_value(cluster, valid_clusters())
      }
      self$config$cluster <- cluster
    },

    cluster_load=function(cluster=NULL) {
      web_shownodes(if (is.null(cluster)) self$config$cluster else cluster)
    },

    submit=function(task_ids) {
      ## See below:
      submit(self, task_ids)
    },
    unsubmit=function(task_ids) {
      unsubmit(self, task_ids)
    }
  ))

submit <- function(obj, task_ids) {
  db <- context::context_db(obj)
  root <- obj$context$root
  config <- obj$config

  f <- function(id) {
    batch <- write_batch(root, id, config, obj$workdir)
    remote_path(prepare_path(batch, config$shares))
  }
  path <- vapply(task_ids, f, character(1), USE.NAMES=FALSE)

  ## TODO: the name must be a scalar (unless Wes has updated it), so
  ## there's no meaninful name that can be added unless
  name <- if (length(task_ids) == 1) task_ids else ""

  dide_id <- web_submit(path, config, paste(task_ids, collapse="\n"))

  for (i in seq_along(task_ids)) {
    db$set(task_ids[[i]], dide_id[[i]],   "dide_id")
    db$set(task_ids[[i]], config$cluster, "dide_cluster")
  }
}

unsubmit <- function(obj, task_ids) {
  db <- context::context_db(obj)
  dide_id <- vcapply(task_ids, db$get, "dide_id")
  dide_cluster <- vcapply(task_ids, db$get, "dide_cluster")

  ## It's possible this is vectorised.
  ret <- vector("list", length(task_ids))
  for (i in seq_along(task_ids)) {
    id <- task_ids[[i]]
    dide_id <- db$get(id, "dide_id")
    cluster <- db$get(id, "dide_cluster")
    ret[[i]] <- web_cancel(cluster, dide_id)
  }
  ret
}
