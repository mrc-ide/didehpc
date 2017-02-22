## * web_login: not needed
## * web_logout: not needed
## * web_submit: via "job submit"
## * web_cancel: via "job cancel"
## * web_shownodes via "node list"
## * web_jobstatus: via "job list"
## * web_joblog: via "job view" or "task view", but needs submission to test

## Use Microsoft HPC tools if installed
has_hpctools <- function() {
  is_windows() && unname(Sys.which("job")) != ""
}

hpc_args <- function(subcommand, ..., args=list(...)) {
  str <- vcapply(args, identity)
  nms <- names(args)
  if (!is.null(nms)) {
    i <- nzchar(names(args))
    str[i] <- sprintf("/%s:%s", nms[i], str[i])
  }
  c(subcommand, unname(str))
}

hpc_run <- function(cmd, ...) {
  res <- suppressWarnings(
    system2(Sys.which(cmd), hpc_args(...), stdout=TRUE, stderr=TRUE))
  status <- attr(res, "status") %||% 0L
  if (status != 0L) {
    stop("Error running command:\n", res)
  }
  res
}

hpc_submit <- function(config, path, name) {
  if (any(!file.exists(path))) {
    stop("All paths must exist")
  }
  args <- list(scheduler=config$cluster,
               jobtemplate=config$template)
  if (!is.null(name)) {
    args$jobname <- name
  }
  if (config$resource$type == "Cores") {
    n <- config$resource$count
    args$numcores <- sprintf("%s-%s", n, n)
  } else {
    args$singlenode <- "true"
  }

  ret <- hpc_run("job", "submit", args=c(args, path))
  parse_job_submit(ret, length(path))
}

hpc_cancel <- function(cluster, dide_task_id) {
  res <- tryCatch(hpc_run("job", "cancel", dide_task_id, scheduler=cluster),
                  error=function(e) e)
  ## TODO: This seems unlikely to be correct...
  if (identical(res, character(0))) {
    ret <- "OK"
  } else {
    ret <- "DID_NOT_CANCEL"
  }
  ret
}

hpc_shownodes <- function(cluster) {
  ## Unfortunately this does not return with an invalid exit code on
  ## incorrect input :(
  res <- hpc_run("node", "listcores", scheduler=cluster)
  parse_node_listcores(res, cluster)
}

hpc_load <- function() {
  stop("Overall load is not supported with hpctools")
}

hpc_jobstatus <- function(config, state) {
  ## NOTE: State can be comma delimited, should support that...
  cluster <- config$cluster
  res <- hpc_run("job", "list", scheduler=cluster, state=state, format="list")

  last <- res[[length(res)]]
  re <- "^([0-9]*)\\s*.*$"
  if (grepl(re, last)) {
    n <- as.integer(sub(re, "\\1", last))
  } else {
    stop("Can't parse response")
  }

  ## This is a total hack but seems the simplest approach at the moment:
  txt <- strsplit(paste(res, collapse="\n"), "\n\n+")[[1]]
  if (length(txt) != n + 1L) {
    stop("Unexpected response length")
  }

  tr <- c(dide_task_id="Id",
          name="Name",
          user="Owner",
          status="State")

  if (n == 1L) {
    ret <- as.data.frame(matrix(character(0), 0, length(tr)),
                         stringsAsFactors=FALSE)
    names(ret) <- names(tr)
  } else {
    f <- function(x) {
      x <- rematch::re_match("^(?<key>[^:]+?)\\s+:\\s+(?<value>.*)\\s*$", x)
      x <- setNames(x[, "value"], x[, "key"])
      setNames(x[tr], names(tr))
    }
    dat <- lapply(strsplit(txt[-length(txt)], "\n"), f)
    ret <- matrix(unlist(dat), length(dat), length(tr), byrow=TRUE)
    colnames(ret) <- names(tr)
    as.data.frame(ret, stringsAsFactors=FALSE)
  }
  ret
}

hpc_joblog <- function(config, dide_task_id) {
  cluster <- config$cluster
  res <- hpc_run("task", "view", paste0(dide_task_id, ".1"), scheduler=cluster)
  i <- grep("^Output\\s*:\\s*$", res)
  if (length(i) < 1) {
    stop("Error parsing log")
  }
  ret <- paste(res[-seq_len(i[[1]])], collapse="\n")
  class(ret) <- "dide_log"
  ret
}
