##' Client for the DIDE cluster web interface.
##'
##' @title DIDE cluster web client
##'
##' @export
web_client <- R6::R6Class(
  "web_client",
  cloneable = FALSE,

  public = list(
    ##' @description Create an API client for the DIDE cluster
    ##'
    ##' @param credentials Either your username, or a list with at least
    ##'   the element `username` and possibly the name `password. If not
    ##'   given, your password will be prompted for at login.
    ##'
    ##' @param cluster_default The default cluster to use; this can be
    ##'   overridden in any command.
    ##'
    ##' @param login Logical, indicating if we should immediately login
    initialize = function(credentials, cluster_default = "fi--dideclusthn",
                          login = FALSE) {
      private$client <- api_client$new(credentials)
      private$cluster <- cluster_name(cluster_default)
      if (login) {
        self$login()
      }
    },

    ##' @description Log in to the cluster
    ##'
    ##' @param refresh Logical, indicating if we should login even if
    ##'   it looks like we are already (useful if login has expired)
    login = function(refresh = TRUE) {
      private$client$login(refresh = refresh)
    },

    ##' @description Log the client out
    logout = function() {
      private$client$GET("/logout.php")
      invisible(TRUE)
    },

    ##' @description Test whether the client is logged in, returning `TRUE`
    ##'   or `FALSE`.
    logged_in = function() {
      r <- private$client$POST("/_listheadnodes.php",
                               httr::accept("text/plain"),
                               body = list(user = encode64("")))
      httr::status_code(r) < 300
    },

    ##' @description Submit a job to the cluster
    ##'
    ##' @param path to the job to submit. This must be a windows (UNC) network
    ##'   path, starting with two backslashes, and must be somewhere that
    ##'    the cluster can see.
    ##'
    ##' @param template The name of the template to use
    ##'
    ##' @param cluster The cluster to submit to, defaulting to the value
    ##'   given when creating the client.
    ##'
    ##' @param resource_type The type of resource to request (either `Cores`
    ##'   or `Nodes`)
    ##'
    ##' @param resource_count The number of resources to request
    submit = function(path, name, template, cluster = NULL,
                      resource_type = "Cores", resource_count = 1) {
      data <- client_submit_body(
        path, name, template, cluster %||% private$cluster,
        resource_type, resource_count)
      r <- private$client$POST("/submit_1.php", httr::accept("text/plain"),
                               body = data)
      client_parse_submit(httr_text(r), 1L)
    },

    ##' @description Cancel a cluster task
    ##'
    ##' @param dide_id The DIDE task id for the task
    ##'
    ##' @param cluster The cluster that the task is running on, defaulting to
    ##'   the value given when creating the client.
    ##'
    ##' @value A named character vector with a status reported by the
    ##'   cluster head node. Names will be the values of `dide_id`
    ##'   and values one of `OK`, `NOT_FOUND`, `WRONG_USER`, `WRONG_STATE`,
    ##'   `ID_ERROR`
    cancel = function(dide_id, cluster = NULL) {
      if (length(dide_task) == 0L) {
        stop("Need at least one task to cancel")
      }
      data <- client_cancel_body(dide_id, cluster %||% private$cluster)
      r <- private$client$POST("/cancel.php", httr::accept("text/plain"),
                               body = data)
      client_parse_cancel(httr_text(r))
    },

    ##' @description Get log from job
    ##'
    ##' @param dide_id The DIDE task id for the task
    ##'
    ##' @param cluster The cluster that the task is running on, defaulting to
    ##'   the value given when creating the client.
    log = function(dide_id, cluster = NULL) {
      assert_scalar_character(dide_id)
      data <- list(hpcfunc = "showfail",
                   cluster = encode64(cluster %||% private$cluster),
                   id = dide_id)
      r <- private$client$POST("/showjobfail.php", httr::accept("text/plain"),
                               body = data)
      client_parse_log(httr_text(r))
    },

    ##' @description Return job status
    ##'
    ##' @param state The state the job is in. Can be one of `Running`,
    ##'   `Finished`, `Queued`, `Failed` or `Cancelled`. Or give `*`
    ##'   for all states (this is the default).
    ##'
    ##' @param cluster The cluster to query, defaulting to the value
    ##'   given when creating the client.
    status = function(state = "*", cluster = NULL) {
      valid <- c("*", "Running", "Finished", "Queued", "Failed", "Cancelled")
      state <- match_value(state, valid)
      data <- list(user = encode64(private$client$username()),
                   scheduler = encode64(cluster %||% private$cluster),
                   state = encode64(state),
                   jobs = encode64(as.character(-1)))
      r <- private$client$POST("/_listalljobs.php", httr::accept("text/plain"),
                               body = data)
      client_parse_status(httr_text(r))
    },

    ##' @description Return an overall measure of cluster use, one
    ##' entry per node within a cluster.
    ##'
    ##' @param cluster The cluster to query, defaulting to
    ##'   the value given when creating the client.
    load_node = function(cluster = NULL) {
      cluster <- cluster %||% private$cluster
      data <- list(cluster = encode64(cluster %||% private$cluster),
                   hpcfunc = "shownodes",
                   cluster_no = as.character(match(cluster, valid_clusters())))
      r <- private$client$POST("/shownodes.php", httr::accept("text/plain"),
                               body = data)
      client_parse_load_cluster(httr_text(r), cluster)
    },

    ##' @description Return an overal measure of cluster use, one
    ##' entry per cluster that you have access to.
    load_overall = function() {
      dat <- lapply(self$headnodes(), self$load_node)
      client_parse_load_overall(dat)
    },

    ##' @description Return a vector of known cluster headnodes. Typically
    ##'   [didehpc::valid_clusters()] will be faster. This endpoint can
    ##'   be used as a relativelyly fast "ping" to check that you are
    ##'   logged in the client and server are talking properly.
    headnodes = function() {
      r <- private$client$POST("/_listheadnodes.php",
                               httr::accept("text/plain"),
                               body = list(user = encode64("")))
      client_parse_headnodes(httr_text(r))
    },

    ##' @description Return a vector of all available R versions
    r_versions = function() {
      r <- private$client$GET("/api/v1/cluster_software/", public = TRUE)
      client_parse_r_versions(httr_text(r))
    },

    ##' @description Returns the low-level api client for debugging
    api_client = function() {
      private$client()
    }
  ),

  private = list(
    client = NULL,
    cluster = NULL
  ))


api_client <- R6::R6Class(
  "api_client",
  cloneable = FALSE,

  public = list(
    initialize = function(credentials, cluster_default) {
      private$credentials <- dide_credentials(credentials, FALSE)
    },

    username = function() {
      private$credentials$username
    },

    GET = function(...) {
      self$request(httr::GET, ...)
    },

    POST = function(...) {
      ## If login will work with accept:plain then we can add this here
      self$request(httr::POST, ..., encode = "form")
    },

    request = function(verb, path, ..., public = FALSE) {
      self$login(public)
      url <- paste0(private$url, path)
      r <- verb(url, ...)
      if (!public && httr::status_code(r) >= 300) {
        stop("Please login first")
      }
      httr::stop_for_status(r)
      r
    },

    login = function(public = FALSE, refresh = FALSE) {
      if (public && !refresh) {
        return()
      }
      if (refresh || is.null(private$logged_in)) {
        private$credentials <- dide_credentials(private$credentials, TRUE)
        data <- list(us = encode64(private$credentials$username),
                     pw = encode64(private$credentials$password),
                     hpcfunc = encode64("login"))
        r <- self$POST("/index.php", body = data, public = TRUE)
        txt <- httr_text(r)
        err <- grepl("You don't seem to have any HPC access.", txt,
                     fixed = TRUE)
        if (err) {
          stop("Error logging on")
        }
        private$logged_in <- TRUE
      }
    }
  ),

  private = list(
    url = "https://mrcdata.dide.ic.ac.uk/hpc",
    credentials = NULL,
    logged_in = FALSE
  ))


client_submit_body <- function(path, name, template, cluster,
                               resource_type, resource_count) {
  ## TODO: this clearly used to allow batch submission of several jobs
  ## at once, and we should consider re-allowing that.
  assert_scalar_character(path)
  if (!grepl("^\\\\\\\\", path)) {
    stop("All paths must be Windows network paths")
  }

  path_call <- paste("call", shQuote(path, "cmd"))

  if (is.null(name)) {
    name <- ""
  } else if (length(name) != 1L) {
    stop("Incorrect number of names")
  }

  workdir <- ""
  stderr <- ""
  stdout <- ""
  list(
    cluster = encode64(cluster),
    template = encode64(template),
    rc = encode64(as.character(resource_count)),
    rt = encode64(resource_type),
    jn = encode64(name),
    wd = encode64(workdir),
    se = encode64(stderr),
    so = encode64(stdout),
    jobs = encode64(path_call),
    dep = encode64(""),
    hpcfunc = "submit")
}


client_cancel_body <- function(dide_id, cluster) {
  jobs <- setNames(as.list(dide_id), paste0("c", dide_id))
  c(list(cluster = encode64(cluster),
         hpcfunc = encode64("cancel")),
    jobs)
}


client_parse_status <- function(txt) {
  cols <- c("dide_id", "name", "status", "resources", "user",
            "time_start", "time_submit", "time_end", "template")
  ## Id Name State Resources User StartTime SubmitTime EndTime JobTemplate
  if (nzchar(txt)) {
    res <- strsplit(strsplit(txt, "\n")[[1]], "\t")
    len <- lengths(res)
    if (any(len != length(cols))) {
      stop("Parse error; unexpected output from server")
    }
    res <- as.data.frame(do.call(rbind, res), stringsAsFactors = FALSE)
  } else {
    res <- as.data.frame(matrix(character(0), 0, length(cols)),
                         stringsAsFactors = FALSE)
  }
  names(res) <- cols

  ## Some type switches:
  res$dide_id <- res$dide_id
  res$name <- trimws(res$name)
  res$name[!nzchar(res$name)] <- NA_character_
  res$user <- sub("^DIDE\\\\", "", res$user)
  res$status <- status_map(res$status)
  res$time_start <- dide_time_parse(res$time_start)
  res$time_end <- dide_time_parse(res$time_end)
  res$time_submit <- dide_time_parse(res$time_submit)
  res
}


client_parse_log <- function(txt) {
  xml <- xml2::read_html(txt)
  value <- xml2::xml_attr(xml2::xml_find_first(xml, '//input[@id="res"]'),
                          "value")
  value <- decode64(value)
  value <- sub("^Output\\s*:\\s*?\n", "", value)
  class(value) <- "dide_log"
  value
}


client_parse_r_versions <- function(txt) {
  dat <- from_json(txt)
  dat_r <- dat$software[vcapply(dat$software, "[[", "name") == "R"]
  numeric_version(vcapply(dat_r, "[[", "version"))
}


client_parse_headnodes <- function(txt) {
  dat <- strsplit(txt, "\n")[[1]]
  stopifnot(all(grepl("^fi--", dat)))
  setdiff(dat, "fi--didelxhn")
}


client_parse_submit <- function(txt, n) {
  txt <- strsplit(txt, "\n")[[1]]
  re <- "^Job has been submitted. ID: +([0-9]+)\\.$"
  i <- grepl(re, txt)

  extra <- txt[!i]
  if (length(extra) > 0L) {
    warning(paste(extra, collapse = "\n"), immediate. = TRUE)
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


client_parse_cancel <- function(txt) {
  d <- strsplit(txt, "\n")[[1]]
  d <- strsplit(d[nzchar(d)], "\t")
  setNames(vcapply(d, "[[", 2L), vcapply(d, "[[", 1L))
}


client_parse_load_cluster <- function(txt, cluster) {
  cluster <- tolower(cluster)
  txt <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  re <- "^([^ ]+) +- +([0-9]+) +([^ ]+) *(.*)$"
  d <- txt[-(1:2)]
  d <- d[nzchar(d)]
  node <- sub(re, "\\1", d)
  core <- as.integer(sub(re, "\\2", d)) + 1L
  status <- sub(re, "\\3", d)
  rest <- sub(re, "\\4", d)
  task_id <- rep(NA_character_, length(d))
  i <- nchar(rest) > 0L
  task_id[i] <- sub("^([0-9]+).*", "\\1", rest[i])
  res <- data.frame(node = tolower(node), core = core, status = status,
                    dide_id = task_id, stringsAsFactors = FALSE)
  res <- res[res$node != cluster, ]
  res <- res[order(res$node), ]
  free <- tapply(res$status == "Idle", res$node, sum)
  total <- tapply(res$node, res$node, length)
  used <- total - free
  percent_used <- round(used / total * 100)

  summary <- data.frame(name = names(free), free = unname(free),
                        used = unname(used),
                        total = unname(total),
                        percent_used = unname(percent_used),
                        stringsAsFactors = FALSE)

  overall <- list(name = cluster,
                  free = sum(free),
                  used = sum(total) - sum(free),
                  total = sum(total),
                  percent_used = round((1 - sum(free) / sum(total)) * 100))

  ret <- list(cluster = cluster, detail = res,
              summary = summary, overall = overall)
  class(ret) <- "dide_clusterload"
  ret
}


client_parse_load_overall <- function(dat) {
  summary <- do.call("rbind", lapply(dat, function(x)
    as.data.frame(x$overall, stringsAsFactors = FALSE)))
  overall <- list(name = "didehpc",
                  free = sum(summary$free),
                  used = sum(summary$used),
                  total = sum(summary$total))
  overall$percent_used <- round(100 * overall$used / overall$total)
  ret <- list(cluster = "didehpc",
              detail = NULL,
              summary = summary,
              overall = overall)
  class(ret) <- "dide_clusterload"
  ret
}


status_map <- function(x, reverse = FALSE) {
  map <- c(Running = "RUNNING",
           Finished = "COMPLETE",
           Queued = "PENDING",
           Failed = "ERROR",
           Canceled = "CANCELLED",
           Cancelled = "CANCELLED") # I don't think the cluster gives this.
  ## for reverse,
  ##   MISSING -> NA
  ##   REDIRECT -> ?
  if (reverse) {
    unname(map[match(x, names(map))])
  } else {
    unname(map[x])
  }
}
