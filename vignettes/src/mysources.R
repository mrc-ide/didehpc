make_tree <- function(nspp) {
  message("I am building a tree!")
  ape::rtree(nspp)
}

combine <- function(a, b, c) {
  sprintf("%s: %2.5f", a, b + c)
}
