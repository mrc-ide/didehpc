##' Getting credentials without storing anything anywhere is
##' difficult, especially without storing information.  We try here.
##'
##' @title Log on to the cluster
##'
##' @param credentials Either a list with elements username, password,
##'   or a path to a file containing lines \code{username=<username>}
##'   and \code{password=<password>} or your username (in which case
##'   you will be prompted graphically for your password).
##'
##' @export
login <- function(credentials) {
  ## What should be stored (and reused if the connection expires) is
  ## the *call* so that we can rerun back through this.
  ##
  ## TODO: It would be great if the website returned a non 3xx code on
  ## login failure.
  dat <- get_credentials(credentials)
  data <- list(us=dat$username, pw=dat$password, hpcfunc=encode64("login"))
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/index.php",
                  curl_insecure(), body=data, encode="form")
  httr::stop_for_status(r)
  txt <- httr::content(r, as="text")

  ## TODO: grep on HTML is terrible but there's no id or anything to
  ## hook against here.
  if (grepl("You don't seem to have any HPC access.", txt, fixed=TRUE)) {
    stop("Error logging on")
  }
  invisible(TRUE)
}

##' @export
##' @rdname login
logout <- function() {
  r <- httr::GET("https://mrcdata.dide.ic.ac.uk/hpc/index.php")
  httr::stop_for_status(r)
  invisible(TRUE)
}

##' Submit tasks to the cluster
##' @title Submit tasks to the cluster
##'
##' @param task Filenames, as \emph{network paths}.  The function
##'   \code{\link{dide_home}} can construct paths to your home
##'   directory easily.
##'
##' @param name Optional name for the task (scalar)
##' @param cluster Cluster to use
##' @export
submit <- function(task, name=NULL, cluster=NULL) {
  if (length(task) == 0L) {
    stop("Must specify at least one task")
  } else {
    assert_character(task)
  }
  err <- !grepl("^\\\\\\\\", task)
  if (any(err)) {
    stop("All tasks must be Windows network paths")
  }
  if (is.null(cluster)) {
    cluster <- valid_clusters()[[1]]
  } else {
    cluster <- match_value(cluster, valid_clusters())
  }
  if (is.null(name)) {
    name <- ""
  } else {
    assert_scalar_character(name)
  }
  template <- "GeneralNodes" # or "4Core", "8Core"
  resource_count <- 1L
  resource_type <- "Cores"
  workdir <- ""
  stderr <- ""
  stdout <- ""
  data <- list(
    cluster=encode64(cluster),
    template=encode64(template),
    rc=encode64(as.character(resource_count)),
    rt=encode64(resource_type),
    jn=encode64(name),
    wd=encode64(workdir),
    se=encode64(stderr),
    sd=encode64(stdout),
    jobs=encode64(paste(task, collapse="\n")),
    hpcfunc="submit")
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/submit_1.php",
                  curl_insecure(), body=data, encode="form")
  httr::stop_for_status(r)
  txt <- httr::content(r, as="text")
  ## This is going to need work in the case where the returned thing
  ## does not return correctly.
  x <- xml2::xml_find_one(xml2::read_html(txt), '//*[@id="res"]/@value')
  res <- decode64(xml2::xml_text(x))
  re <- "^Job has been submitted. ID: +([0-9]+)\\.\n$"
  if (grepl(re, res)) {
    id <- as.integer(sub(re, "\\1", res))
    list(name=name, id=id)
  } else {
    ## recover
  }
}

##' Construct network path to files in your home directory.
##' @title Network path construction
##' @param path Path, relative to your home direcory
##' @param username Your username
##' @export
dide_home <- function(path, username) {
  assert_scalar_character(username)
  assert_character(path)
  paste0("\\\\fi--san02\\homes\\", username, "\\", gsub("/", "\\\\", path))
}

##' Get status of the nodes.  This is just a holding place until the
##' website can return something like JSON.
##' @title Node status
##' @param cluster Name of the cluster
##' @export
shownodes <- function(cluster=NULL) {
  ## TODO: This doesn't get the node names because reading HTML from
  ## tables is unpleasant.
  if (is.null(cluster)) {
    cluster <- valid_clusters()[[1]]
  } else {
    cluster <- match_value(cluster, valid_clusters())
  }
  data <- list(cluster=encode64(cluster),
               hpcfunc="shownodes",
               cluster_no=as.character(match(cluster, valid_clusters())))
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/shownodes.php",
                  curl_insecure(), body=data, encode="form")
  httr::stop_for_status(r)
  txt <- httr::content(r, as="text")
  ## If this returned JSON this would be heaps easier.  But we'll just
  ## jump right in and parse this, even if we just do a crap job.  Not
  ## sure what the failure mode is where we're not validly logged in
  ## though.
  html <- xml2::read_html(txt)
  xx <- vapply(xml2::xml_find_all(html, "//table//table//img/@alt"),
               xml2::xml_text, character(1))
  re <- "([0-9]+)/([0-9]+) in use"
  y1 <- as.integer(sub(re, "\\1", xx[grepl(re, xx)]))
  y2 <- as.integer(sub(re, "\\2", xx[grepl(re, xx)]))

  ## Pull this apart into groups:
  z1 <- z2 <- integer()
  i <- 1L
  while (i < length(y1)) {
    z1 <- c(z1, y1[[i]])
    z2 <- c(z2, y2[[i]])
    i <- i + y2[[i]]
  }
  cbind(used=z1, total=z2)
}

valid_clusters <- function() {
  c("fi--dideclusthn", "fi--didemrchnb")
}

get_credentials <- function(credentials) {
  ## Jesus.  Some cleaning here to do.
  ## Check that username/password is OK
  ## Perhaps allow and parse environment variables
  if (is.list(credentials)) {
    ## check that username, password are present
    if (!all(c("username", "password") %in% names(credentials))) {
      stop("Invalid")
    }
    ret <- credentials
  } else if (is.character(credentials)) {
    if (file.exists(credentials)) {
      ret <- read_credentials(credentials)
    } else {
      ## Assume we have a username.
      ret <- list(username=credentials,
                  password=password_tcltk(credentials))
    }
  } else {
    stop("Unexpected type")
  }

  ret$username <- sub("^DIDE\\\\", "", ret$username)
  lapply(ret, encode64)
}

## Format is
## username=<username>
## password=<password>
read_credentials <- function(filename) {
  ## Lots of validation needed here.
  dat <- strsplit(readLines(filename), "=")
  setNames(as.list(trimws(vapply(dat, "[[", character(1), 2L))),
           trimws(vapply(dat, "[[", character(1), 1L)))
}

password_tcltk <- function(username) {
  loadNamespace("tcltk")
  wnd <- tcltk::tktoplevel()
  tcltk::tktitle(wnd) <- "Enter password"
  pass_var <- tcltk::tclVar("")
  command <- function() tcltk::tkdestroy(wnd)
  tcltk::tkgrid(tcltk::tklabel(wnd, text=sprintf("Password for %s:", username)))
  tcltk::tkgrid(pass_box <- tcltk::tkentry(wnd, textvariable=pass_var, show="*"))
  tcltk::tkbind(pass_box, "<Return>", command)
  tcltk::tkgrid(tcltk::tkbutton(wnd, text="OK", command=command))
  tcltk::tkfocus(pass_box)
  tcltk::tkwait.window(wnd)
  invisible(tcltk::tclvalue(pass_var))
}

## This probably should not be used in OSX's R.app; worth finding out.
## switch on
##   .Platform$GUI
## I think.
password_unix <- function(username) {
  stty <- Sys_which("stty")
  cat(sprintf("Password for %s: ", username))
  pass <- system('stty -echo && read ff && stty echo && echo $ff && ff=""',
                 intern=TRUE)
  cat('\n')
  invisible(pass)
}
