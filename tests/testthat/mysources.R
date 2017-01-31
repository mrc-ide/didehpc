make_tree <- function(nspp) {
  message("I am building a tree!")
  ## Sys.sleep(10)
  ape::rtree(nspp)
}
