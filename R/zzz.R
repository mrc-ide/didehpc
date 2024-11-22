.onLoad <- function(...) {
  inform()
  if (!identical(Sys.getenv("IN_PKGDOWN"), "true")) {
    stop("The didehpc package is no longer functional", call. = FALSE)
  }
}


show_paragraph <- function(x) {
  message(paste0(x, "\n", collapse = ""))
}


inform <- function() {
  intro <- c(" -----",
             "  The future is here and it is hipercow",
             " ------",
             "    \\   ^__^",
             "     \\  (oo)\\ ________",
             "        (__)\\         )\\ /\\",
             "             ||------w|",
             "             ||      ||")
  show_paragraph(intro)

  str <- "You have tried to install and load 'didehpc', which was our cluster toolkit from 2015 to 2023.  This has now been replaced entirely by hipercow, which is really much better."
  show_paragraph(strwrap(str))


  str <- "If you are migrating from 'didehpc' (or have old code you are trying to understand), please see the migration vignette: https://mrc-ide.github.io/hipercow/articles/migration.html"
  show_paragraph(strwrap(str))

  show_paragraph("You can install hipercow by running:")

  install <- c(
    'install.packages(',
    '  "hipercow",',
    '  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))')
  show_paragraph(install)
}
