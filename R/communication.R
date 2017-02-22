## TODO: dide_task_id -> dide_id everywhere as it's already used in
## the db and inteface

didehpc_submit <- function(config, path, name=NULL) {
  assert_scalar_character(path)
  if (!is.null(name)) {
    assert_scalar_character(name)
  }

  if (use_hpctools(config)) {
    hpc_submit(config, path, name)
  } else {
    web_submit(config, path, name)
  }
}

## NOTE: this is the *dide_task_id*, not our ID.  Do the lookup elsewhere.
didehpc_cancel <- function(config, dide_task_id) {
  if (length(dide_task_id) == 0L) {
    stop("Need at least one task to cancel")
  }
  if (length(dide_task_id) == 0L) {
    stop("Need at least one task to cancel")
  }
  cluster <- config$cluster
  if (use_hpctools(config)) {
    hpc_cancel(cluster, dide_task_id)
  } else {
    web_cancel(cluster, dide_task_id)
  }
}

didehpc_shownodes <- function(config, cluster=NULL) {
  if (is.null(cluster)) {
    cluster <- getOption("didehpc.cluster", valid_clusters()[[1]])
  } else {
    cluster <- match_value(cluster, valid_clusters())
  }
  if (use_hpctools(config)) {
    hpc_shownodes(cluster)
  } else {
    web_shownodes(cluster)
  }
}

didehpc_load <- function(config) {
  if (use_hpctools(config)) {
    hpc_load()
  } else {
    web_load()
  }
}

didehpc_jobstatus <- function(config, state="*", n=Inf) {
  valid <- c("*", "Running", "Finished", "Queued", "Failed", "Cancelled")
  state <- match_value(state, valid)
  if (use_hpctools(config)) {
    hpc_jobstatus(config, state)
  } else {
    web_jobstatus(config, state)
  }
}

## This one is not available via the HPC tools I think.  Check with
## Wes though (I don't see it in the GUI either).
didehpc_joblog <- function(config, dide_task_id) {
  if (use_hpctools(config)) {
    hpc_joblog(config, dide_task_id)
  } else {
    web_joblog(config, dide_task_id)
  }
}

use_hpctools <- function(config) {
  isTRUE(config$hpctools)
}

##' @export
print.dide_log <- function(x, ...) {
  ## Deal with newline issues:
  value <- sub("\n*$", "", gsub("\n\n", "\n", x))
  ## TODO: might be worth some processing here...
  ## re <- "(.*?)Output\\s*:\\n(.*)"
  ## pre <- sub(re, "\\1", value)
  ## ret <- sub(re, "\\2", value)
  ## if (nzchar(pre)) {
  ##   attr(ret, "message") <- pre
  ## }
  ## ret
  cat(paste0(value, "\n"))
}

parse_node_listcores <- function(txt, cluster) {
  cluster <- tolower(cluster)
  re <- "^([^ ]+) +- +([0-9]+) +([^ ]+) *(.*)$"
  d <- txt[-(1:2)]
  node <- sub(re, "\\1", d)
  core <- as.integer(sub(re, "\\2", d)) + 1L
  status <- sub(re, "\\3", d)
  rest <- sub(re, "\\4", d)
  task_id <- rep(NA_character_, length(d))
  i <- nchar(rest) > 0L
  task_id[i] <- sub("^([0-9]+).*", "\\1", rest[i])
  res <- data.frame(node=tolower(node), core=core, status=status,
                    dide_task_id=task_id, stringsAsFactors=FALSE)
  res <- res[res$node != cluster, ]
  res <- res[order(res$node), ]
  free <- tapply(res$status == "Idle", res$node, sum)
  total <- tapply(res$node, res$node, length)
  summary <- data.frame(name=names(free), free=unname(free),
                        used=unname(total - free),
                        total=unname(total), stringsAsFactors=FALSE)

  overall <- list(name=cluster, free=sum(free),
                  used=sum(total) - sum(free), total=sum(total))

  ret <- list(cluster=cluster, detail=res,
              summary=summary, overall=overall)
  class(ret) <- "dide_clusterload"
  ret
}

parse_job_submit <- function(txt, n) {
  re <- "^Job has been submitted. ID: +([0-9]+)\\.$"
  i <- grepl(re, txt)

  extra <- txt[!i]
  if (length(extra) > 0L) {
    warning(paste(extra, collapse="\n"), immediate.=TRUE)
  }

  nok <- sum(i)
  if (nok > 0L) {
    if (nok != n) {
      ## Hopefully never triggers
      stop("Unexpected response length from server")
    }
    sub(re, "\\1", txt[i])
  } else {
    ## TODO: Detect this and hit login and try again?
    stop("Job submission has likely failed; could be a login error")
  }
}

##' @export
print.dide_clusterload <- function(x, ..., nodes=TRUE) {
  ## There's a bit of faff here to get alignments to work nicely.
  f <- function(name) {
    format(c(name, x$overall[[name]], x$summary[[name]]), justify="right")
  }
  m <- cbind(f("name"), f("free"), f("used"), f("total"))

  ## Header:
  mh <- vcapply(m[1, ], crayon::bold)

  ## Divider:
  md <- vcapply(nchar(m[1,]), strrep, x="-")

  ## Summary
  if (nodes) {
    ms <- m[-(1:2), , drop=FALSE]
    col <- cluster_load_cols(x$summary$used / x$summary$total)
    ms[, 1] <- crayon::blue(ms[, 1])
    ms[, -1] <- t(vapply(seq_along(col),
                         function(i) crayon::make_style(col[[i]])(ms[i, -1]),
                         character(ncol(m) - 1L)))
    ms <- rbind(ms, md)
  } else {
    ms <- NULL
  }

  ## Overall
  mo <- m[2, ]
  col <- cluster_load_cols(x$overall$used / x$overall$total)
  mo[1] <- crayon::make_style("blue")$bold(mo[1])
  mo[-1] <- vcapply(mo[-1], crayon::make_style(col)$bold)

  mm <- rbind(mh, md, ms, mo)

  cat(paste0(apply(mm, 1, paste, collapse=" "), "\n", collapse=""))
  invisible(x)
}

cluster_load_cols <- function(p, max=1) {
  ## cols <- c("#A50026", "#D73027", "#F46D43", "#FDAE61",
  ##           "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9",
  ##           "#74ADD1", "#4575B4", "#313695")
  cols <- c("#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")
  ret <- colorRamp(cols)(p / max)
  rgb(ret[, 1], ret[, 2], ret[, 3], maxColorValue=255)
}

status_map <- function(x, reverse=FALSE) {
  map <- c(Running="RUNNING",
           Finished="COMPLETE",
           Queued="PENDING",
           Failed="ERROR",
           Canceled="CANCELLED",
           Cancelled="CANCELLED") # I don't think the cluster gives this.
  ## for reverse,
  ##   MISSING -> NA
  ##   REDIRECT -> ?
  if (reverse) {
    unname(map[match(x, names(map))])
  } else {
    unname(map[x])
  }
}
