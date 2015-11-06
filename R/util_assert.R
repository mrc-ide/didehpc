match_value <- function(arg, choices, name=deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(dQuote(choices), collapse=", ")))
  }
  arg
}
assert_scalar <- function(x, name=deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call.=FALSE)
  }
}
assert_scalar_character <- function(x, name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}
assert_character <- function(x, name=deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("%s must be character", name), call.=FALSE)
  }
}
