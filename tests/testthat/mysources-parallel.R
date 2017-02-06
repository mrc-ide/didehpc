make_tree <- function(nspp) {
  message("building a tree")
  ape::rtree(nspp)
}

make_tree_par <- function(nspp, n) {
  parallel::clusterApply(NULL, seq_len(n), function(.) make_tree(nspp))
}

list_parallel_pids <- function() {
  list(local = Sys.getpid(),
       parallel = parallel::clusterCall(NULL, Sys.getpid))
}
