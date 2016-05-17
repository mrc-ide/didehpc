##' Getting credentials without storing anything anywhere is
##' difficult, especially without storing information.  We try here.
##'
##' The logic here will change as the general configuration approach
##' solidifies.  For now it's all a bit crap.
##'
##' @title Log on to the cluster
##'
##' @param config A \code{\link{didewin_config}} object.  If settings
##'   have been mde global this can be omitted.
##'
##' @export
web_login <- function(config=didewin_config()) {
  ## What should be stored (and reused if the connection expires) is
  ## the *call* so that we can rerun back through this.
  ##
  ## TODO: It would be great if the website returned a non 3xx code on
  ## login failure.
  dat <- get_credentials(config$credentials, TRUE)
  data <- list(us=encode64(dat$username),
               pw=encode64(dat$password),
               hpcfunc=encode64("login"))
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/index.php",
                  curl_insecure(), body=data, encode="form")
  httr::stop_for_status(r)
  txt <- httr::content(r, as="text", encoding="UTF-8")

  ## TODO: grep on HTML is terrible but there's no id or anything to
  ## hook against here.
  if (grepl("You don't seem to have any HPC access.", txt, fixed=TRUE)) {
    stop("Error logging on")
  }
  invisible(TRUE)
}

##' @export
##' @rdname web_login
web_logout <- function() {
  r <- httr::GET("https://mrcdata.dide.ic.ac.uk/hpc/index.php",
                 curl_insecure())
  httr::stop_for_status(r)
  invisible(TRUE)
}

##' Submit tasks to the cluster
##' @title Submit tasks to the cluster
##'
##' @param task Filenames, as \emph{network paths}.
##'
##' @param config Configuration information, via \code{\link{didewin_config}}.
##'
##' @param name Optional name for the task.
##'
##' @export
web_submit <- function(task, config, name="") {
  if (length(task) == 0L) {
    stop("Must specify at least one task")
  } else {
    assert_character(task)
  }
  err <- !grepl("^\\\\\\\\", task)
  if (any(err)) {
    stop("All tasks must be Windows network paths")
  }
  if (length(name) != 1L) {
    stop("name must be a scalar")
  }
  if (length(task) > 1L && nzchar(name)) {
    stop("Can't use name when submitting >1 task")
  }

  if (config$template == "GeneralNodes") {
    resource_count <- 1L
  } else {
    resource_count <- as.integer(sub("Core", "", config$template))
  }

  i <- grepl(" ", task)
  if (any(i)) {
    task[i] <- shQuote(task[i], "cmd")
  }

  workdir <- ""
  stderr <- ""
  stdout <- ""
  data <- list(
    cluster=encode64(config$cluster),
    template=encode64(config$template),
    rc=encode64(as.character(config$resource$count)),
    rt=encode64(config$resource$type),
    jn=encode64(name),
    wd=encode64(workdir),
    se=encode64(stderr),
    so=encode64(stdout),
    jobs=encode64(paste(task, collapse="\n")),
    dep=encode64(""),
    hpcfunc="submit")

  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/submit_1.php",
                  curl_insecure(),
                  httr::accept("text/plain"),
                  body=data, encode="form")
  httr::stop_for_status(r)

  txt <- httr::content(r, as="text", encoding="UTF-8")
  res <- strsplit(txt, "\n")[[1]]
  re <- "^Job has been submitted. ID: +([0-9]+)\\.$"
  i <- grepl(re, res)

  extra <- res[!i]
  if (length(extra) > 0L) {
    warning(paste(extra, collapse="\n"), immediate.=TRUE)
  }

  nok <- sum(i)
  if (nok > 0L) {
    if (nok != length(task)) {
      ## Hopefully never triggers
      stop("Unexpected response length from server")
    }
    sub(re, "\\1", res[i])
  } else {
    ## TODO: Detect this and hit login and try again?
    stop("Job submission has likely failed; could be a login error")
  }
}

##' Get status of the nodes.  This is just a holding place until the
##' website can return something like JSON.
##' @title Node status
##' @param cluster Name of the cluster
##' @export
web_shownodes <- function(cluster=NULL) {
  ## TODO: This doesn't get the node names because reading HTML from
  ## tables is unpleasant.
  if (is.null(cluster)) {
    cluster <- getOption("didewin.cluster", valid_clusters()[[1]])
  } else {
    cluster <- match_value(cluster, valid_clusters())
  }
  data <- list(cluster=encode64(cluster),
               hpcfunc="shownodes",
               cluster_no=as.character(match(cluster, valid_clusters())))
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/shownodes.php",
                  curl_insecure(),
                  httr::accept("text/plain"),
                  body=data, encode="form")
  check_status(r)
  txt <- httr::content(r, as="text", encoding="UTF-8")
  dat <- strsplit(txt, "\n")[[1]]
  re <- "^([^ ]+) +- +([0-9]+) +([^ ]+) *(.*)$"
  d <- dat[-(1:2)]
  node <- sub(re, "\\1", d)
  core <- as.integer(sub(re, "\\2", d)) + 1L
  status <- sub(re, "\\3", d)
  rest <- sub(re, "\\4", d)
  task_id <- rep(NA_character_, length(d))
  i <- nchar(rest) > 0L
  task_id[i] <- sub("^([0-9]+).*", "\\1", rest[i])
  data.frame(node=node, core=core, status=status, dide_task_id=task_id,
             stringsAsFactors=FALSE)
}

## NOTE: this is the *dide_task_id*, not our ID.  Do the lookup elsewhere.
web_cancel <- function(cluster, dide_task_id) {
  if (is.null(cluster)) {
    cluster <- getOption("didewin.cluster", valid_clusters()[[1]])
  } else {
    cluster <- match_value(cluster, valid_clusters())
  }
  if (length(dide_task_id) == 0L) {
    stop("Need at least one task to cancel")
  }
  jobs <- setNames(as.list(dide_task_id), paste0("c", dide_task_id))
  data <- c(list(cluster=encode64(cluster),
                 hpcfunc=encode64("cancel")),
            jobs)
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/cancel.php",
                  curl_insecure(),
                  httr::accept("text/plain"),
                  body=data, encode="form")
  check_status(r)
  txt <- httr::content(r, as="text", encoding="UTF-8")
  ## Possibilities here are:
  ##   - OK
  ##   - NOT_FOUND
  ##   - WRONG_USER
  ##   - WRONG_STATE
  ##   - ID_ERROR
  txt
}

##' Get job status from the cluster
##' @title Job status
##'
##' @param x Either a username or configuration object
##'
##' @param cluster The cluster to check (default taken from
##'   configuration if passed as \code{x}).
##'
##' @param state State of jobs to check; default is all states ("*").
##'   Other valid options are "Running", "Finished", "Queued",
##'   "Failed" and "Cancelled".
##'
##' @param n number of rows to return (default is Inf, which returns
##'   all available jobs).
##'
##' @export
web_jobstatus <- function(x, cluster=valid_clusters()[[1]],
                          state="*", n=Inf) {
  if (inherits(x, "didewin_config")) {
    username <- x$username
    if (missing(cluster)) {
      cluster <- x$cluster
    }
  } else {
    assert_scalar_character(x)
    username <- x
  }
  cluster <- match_value(cluster, valid_clusters())
  valid <- c("*", "Running", "Finished", "Queued", "Failed", "Cancelled")
  state <- match_value(state, valid)
  if (n == Inf) {
    n <- -1
  } else {
    if (!is.finite(n) || n <= 0) {
      stop("n must be a positive integer (including Inf)")
    }
  }
  data <- list(user=encode64(username),
               scheduler=encode64(cluster),
               state=encode64(state),
               jobs=encode64(as.character(n)))
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/_listalljobs.php",
                  curl_insecure(),
                  httr::accept("text/plain"),
                  body=data, encode="form")
  check_status(r)
  txt <- httr::content(r, as="text", encoding="UTF-8")

  cols <- c("dide_task_id", "name", "status", "resources", "user",
            "time_start", "time_submit", "time_end", "template")
  ## Id Name State Resources User StartTime SubmitTime EndTime JobTemplate
  if (nzchar(txt)) {
    res <- strsplit(strsplit(txt, "\n")[[1]], "\t")
    len <- lengths(res)
    if (any(len != length(cols))) {
      stop("Parse error; unexpected output from server")
    }
    res <- as.data.frame(do.call(rbind, res), stringsAsFactors=FALSE)
  } else {
    res <- as.data.frame(matrix(character(0), 0, length(cols)),
                         stringsAsFactors=FALSE)
  }
  names(res) <- cols

  ## Some type switches:
  res$dide_task_id <- res$dide_task_id
  res$name <- trimws(res$name)
  res$name[!nzchar(res$name)] <- NA_character_
  res$user <- sub("^DIDE\\\\", "", res$user)
  res$status <- status_map(res$status)
  res$time_start <- dide_time_parse(res$time_start)
  res$time_end <- dide_time_parse(res$time_end)
  res$time_submit <- dide_time_parse(res$time_submit)
  res
}

##' Get the log of a command from the HPC server.  Note that logs are
##' only available for jobs that have finished or failed (not availab e
##'
##' @title Get job log
##' @param config A config object
##' @param dide_id A \emph{DIDE} id number
##' @export
web_joblog <- function(config, dide_id) {
  data <- list(hpcfunc="showfail",
               cluster=encode64(config$cluster),
               id=dide_id)
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/showjobfail.php",
                  curl_insecure(),
                  httr::accept("text/plain"),
                  body=data, encode="form")
  check_status(r)
  xml <- xml2::read_html(httr::content(r, as="text", encoding="UTF-8"))
  value <- xml2::xml_attr(xml2::xml_find_one(xml, '//input[@id="res"]'),
                          "value")
  value <- decode64(value)
  class(value) <- "dide_log"
  value
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

status_map <- function(x, reverse=FALSE) {
  map <- c(Running="RUNNING",
           Finished="COMPLETE",
           Queued="PENDING",
           Failed="ERROR",
           Cancelled="ORPHAN")
  ## for reverse,
  ##   MISSING -> NA
  ##   REDIRECT -> ?
  if (reverse) {
    unname(map[match(x, names(map))])
  } else {
    unname(map[x])
  }
}

dide_time_parse <- function(x) {
  ## YYYYMMDDHHMMSS
  ## 20151109170805
  strptime(x, "%Y%m%d%H%M%S")
}

get_credentials <- function(credentials, need_password=TRUE) {
  if (is.null(credentials)) {
    if (!interactive()) {
      stop("Credentials file needed for non-interactive use")
    }
    credentials <- trimws(readline(prompt="DIDE username: "))
    if (credentials == "") {
      stop("Invalid empty username")
    }
  }
  ## Jesus.  Some cleaning here to do.
  ## Check that username/password is OK
  ## Perhaps allow and parse environment variables
  if (is.list(credentials)) {
    ret <- check_credentials(credentials, need_password)
  } else if (is.character(credentials)) {
    if (file.exists(credentials)) {
      ret <- read_credentials(credentials)
    } else {
      ## Assume we have a username.
      ret <- list(username=credentials)
      if (need_password) {
        if (!interactive()) {
          stop("Credentials file needed for non-interactive use")
        }
        ret$password <- getPass::getPass(
          sprintf("Enter DIDE password for %s: ", ret$username))
      }
    }
  } else {
    stop("Unexpected type")
  }

  ret$username <- sub("^DIDE\\\\", "", ret$username)
  ret
}

## Format is
## username=<username>
## password=<password>
read_credentials <- function(filename) {
  dat <- strsplit(readLines(filename), "=")
  dat <- setNames(as.list(trimws(vapply(dat, "[[", character(1), 2L))),
                  trimws(vapply(dat, "[[", character(1), 1L)))
  check_credentials(dat, TRUE)
}

check_credentials <- function(credentials, need_password) {
  if (is.null(names(credentials))) {
    stop("Credentials must be named")
  }
  extra <- setdiff(names(credentials), c("username", "password"))
  if (length(extra) > 0L) {
    stop("Unknown fields in credentials: ", paste(extra, collapse=", "))
  }
  req <- c("username", if (need_password) "password")
  msg <- setdiff(req, names(credentials))
  if (length(msg) > 0L) {
    stop("Missing fields in credentials: ", paste(msg, collapse=", "))
  }
  credentials # consider credentials[req]
}

## It might be worth having another shot here; recalling if the
## configuration is given and if an expression to evaluate is there...
check_status <- function(r) {
  if (httr::status_code(r) >= 300) {
    stop("Please login first with web_login()")
  }
}
