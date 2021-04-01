##' @export
format.dide_clusterload <- function(x, ..., nodes = TRUE) {
  f <- function(name) {
    vals <- c(x$overall[[name]], x$summary[[name]])
    if (name == "percent_used") {
      name <- "% used"
      vals <- paste0(vals, "%")
    }
    format(c(name, vals), justify = "right")
  }
  m <- cbind(f("name"), f("free"), f("used"), f("total"), f("percent_used"))

  ## Header:
  mh <- vcapply(m[1, ], crayon::bold)

  ## Divider:
  md <- vcapply(nchar(m[1,]), strrep, x = "-")

  ## Summary
  if (nodes) {
    ms <- m[-(1:2), , drop = FALSE]
    col <- cluster_load_cols(x$summary$used / x$summary$total)
    ms[, 1] <- crayon::blue(ms[, 1])
    ms[, -1] <- t(vapply(seq_along(col),
                         function(i) crayon::make_style(col[[i]])(ms[i, -1]),
                         character(ncol(m) - 1L)))
    ms <- rbind(ms, md, deparse.level = 0)
  } else {
    ms <- NULL
  }

  ## Overall
  mo <- m[2, ]
  col <- cluster_load_cols(x$overall$used / x$overall$total)
  mo[1] <- crayon::make_style("blue")$bold(mo[1])
  mo[-1] <- vcapply(mo[-1], crayon::make_style(col)$bold)

  mm <- rbind(mh, md, ms, mo, deparse.level = 0)

  apply(mm, 1, paste, collapse = " ")
}


##' @export
print.dide_clusterload <- function(x, ...) {
  cat(paste0(format(x, ...), "\n", collapse = ""))
  invisible(x)
}


cluster_load_cols <- function(p, max = 1) {
  cols <- c("#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")
  p[is.nan(p)] <- 0
  ret <- grDevices::colorRamp(cols)(p / max)
  grDevices::rgb(ret[, 1], ret[, 2], ret[, 3], maxColorValue = 255)
}
