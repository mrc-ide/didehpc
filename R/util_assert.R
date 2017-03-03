match_value <- function(arg, choices, name = deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(dQuote(choices), collapse = ", ")))
  }
  arg
}
assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call. = FALSE)
  }
}
assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}
assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("%s must be character", name), call. = FALSE)
  }
}
assert_value <- function(arg, choices, name = deparse(substitute(arg))) {
  match_value(arg, choices, name)
  invisible(NULL)
}
assert_integer <- function(x, strict = FALSE, name = deparse(substitute(x))) {
  if (!(is.integer(x))) {
    usable_as_integer <-
      !strict && is.numeric(x) && (max(abs(as.integer(x) - x)) < 1e-8)
    if (!usable_as_integer) {
      stop(sprintf("%s must be integer", name), call. = FALSE)
    }
  }
}
assert_scalar_integer <- function(x, strict = FALSE,
                                  name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_integer(x, strict, name)
}
