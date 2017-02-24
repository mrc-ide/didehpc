There are a lot of changes behind the scenes in this update, and it has been unfortunately necessary to break some things in order to support things better going forward.

# Code changes

* `didewin::` becomes `didehpc::`
* `queue_didewin` becomes `queue_didehpc`
* `didewin_config` becomes `didehpc_config`
* `context::package_sources` becomes `provisionr::package_sources` but little changes in how it works
* `lapply` and `enqueue_bulk` move inside the queue object
* `enqueue_bulk` have an argument name change from `do.call` to `do_call`

A few functions changed name

* `$tasks_list()` -> `$task_list()`
* `$tasks_status_dide()` -> `$task_status_dide()`

It will be best if you upgrade the packages by first removing the old packages:

```
remove.packages(c("storr", "context", "queuer", "didewin"))
```

and then install the new ones with either


```r
drat:::add("dide-tools")
install.packages(c("didehpc", "buildr", "syncr"))
```

or

```r
source("https://dide-tools.github.io/didehpc/install")
```

# Other practical changes

The database structure has significantly changed; do we need a migration script?  Otherwise it is time to save what you want from your context and start again in a fresh directory

# New features!

In exchange for this hassle, the core new features are:

* Select an R version.  By default the package will use the most recent R version installed on the cluster but you can select any R version listed on the wiki.
* Support for a new (small) linux cluster.  Talk to Rich and Wes if you'd like to try this out
* A reduction in the number of jobs that get stuck at `PENDING` - these will come back as `ERROR` now.  Jobs that fail to start entirely due to network path mapping are still problematic though
* Experimental support for storing job data in a SQL database.  This is heaps faster than the network disk, but is experimental and subject to change
* Use a shared library of R packages (if you have the temporary directory mounted), which can speed up creating the queue, especially when `BH` is a dependency
* Improvements to using non-CRAN packages, though this still needs to be improved.  Package installation should still be more robust now and has been moved out into its own package (`provisionr`)
* Simpler installation

Most of the rest of the changes are not user visible, but will hopefully make the package more robust.  This has involved a total rewrite of the underlying packages (`context`, `queuer` and optionally `rrq`) and creation of a lot of tests to make sure that things actually behave as expected.
