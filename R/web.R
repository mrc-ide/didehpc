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
  r <- httr::GET("https://mrcdata.dide.ic.ac.uk/hpc/logout.php",
                 curl_insecure())
  httr::stop_for_status(r)
  invisible(TRUE)
}

##' @export
##' @rdname web_login
web_logged_in <- function() {
  ## This *should* work but the website needs tweaking so that it
  ## checks that we're logged in before running this.
  ##
  ##   data <- list(action = "submit.php",
  ##                hpcfunc = "submit",
  ##                cluster = encode64("fi--dideclusthn"),
  ##                cluster_no = "0")
  ##   r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/submit.php",
  ##                   curl_insecure(),
  ##                   httr::accept("text/plain"),
  ##                   body=data, encode="form")
  ##   httr::status_code(r) < 300
  r <- httr::GET("https://mrcdata.dide.ic.ac.uk/hpc/index.php",
                 curl_insecure())
  xml <- httr::content(r, "parsed", encoding = "UTF-8")
  inherits(xml2::xml_find_first(xml, "//form[@name = 'flogin']"),
           "xml_missing")
}

web_submit <- function(config, path, name) {
  if (linux_cluster(config$cluster)) {
    path <- paste("bash", path)
  } else {
    if (any(!grepl("^\\\\\\\\", path))) {
      stop("All paths must be Windows network paths")
    }
  }
  if (is.null(name)) {
    name <- ""
  } else if (length(name) != length(path)) {
    stop("Incorrect number of names")
  } else if (length(name) > 1L) {
    ## TODO: Hack for now, later we'll newline collapse the names, but
    ## that requires a tweak at the cluster level.
    path <- sprintf('/jobname:"%s" "%s"', name, path)
    name <- ""
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
    jobs=encode64(paste(path, collapse="\n")),
    dep=encode64(""),
    hpcfunc="submit")

  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/submit_1.php",
                  curl_insecure(),
                  httr::accept("text/plain"),
                  body=data, encode="form")
  httr::stop_for_status(r)

  txt <- httr::content(r, as="text", encoding="UTF-8")
  res <- strsplit(txt, "\n")[[1]]
  parse_job_submit(res, length(path))
}

## NOTE: this is the *dide_task_id*, not our ID.  Do the lookup elsewhere.
web_cancel <- function(cluster, dide_task_id) {
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
  sub("[0-9]+\t([A-Z]+)\\s+", "\\1", txt)
}

web_shownodes <- function(cluster) {
  data <- list(cluster=encode64(cluster),
               hpcfunc="shownodes",
               cluster_no=as.character(match(cluster, valid_clusters())))
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/shownodes.php",
                  curl_insecure(),
                  httr::accept("text/plain"),
                  body=data, encode="form")
  check_status(r)
  txt <- httr::content(r, as="text", encoding="UTF-8")
  parse_node_listcores(strsplit(txt, "\n")[[1L]], cluster)
}

web_jobstatus <- function(config, state) {
  username <- config$username
  cluster <- config$cluster

  n <- -1
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

web_joblog <- function(config, dide_task_id) {
  data <- list(hpcfunc="showfail",
               cluster=encode64(config$cluster),
               id=dide_task_id)
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/showjobfail.php",
                  curl_insecure(),
                  httr::accept("text/plain"),
                  body=data, encode="form")
  check_status(r)
  xml <- xml2::read_html(httr::content(r, as="text", encoding="UTF-8"))
  value <- xml2::xml_attr(xml2::xml_find_first(xml, '//input[@id="res"]'),
                          "value")
  value <- decode64(value)
  value <- sub("^Output\\s*:\\s*?\n", "", value)
  class(value) <- "dide_log"
  value
}

dide_time_parse <- function(x) {
  ## YYYYMMDDHHMMSS
  ## 20151109170805
  strptime(x, "%Y%m%d%H%M%S")
}

## It might be worth having another shot here; recalling if the
## configuration is given and if an expression to evaluate is there...
check_status <- function(r) {
  if (httr::status_code(r) >= 300) {
    stop("Please login first with web_login()")
  }
}
