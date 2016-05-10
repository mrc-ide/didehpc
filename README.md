# didewin

> DIDE Windows Cluster Support

**WARNING: Nothing in here will stay the same; use at your own risk**

**NOTICE**: This will only be of use to people at DIDE, as it uses our [cluster web portal](https://mrcdata.dide.ic.ac.uk/hpc), local cluster, and local network file systems.

# Usage

At this point, only very basic usage is supported.  The idea will be to factor out much of the code in this package into a general clustering package and this one will contain only the driver for our windows cluster.  But for now it is what it is.  As indicated in the warning, the interface here will change!

## General proceedure

Note that at this point the package is still low level (this is meant to go in the middle of two other packages -- one lower level and one nice user interface package) but it should still work OK.

The steps below are described in more detail in the usage section:

1. Ensure that your project is in a directory that the cluster can see (i.e. on one of the network drives).  See [notes](#setting-up-network-shares) at the bottom of this file for instructions
2. Set your DIDE credentials up so that you can log in and tell `didewin` about them.
3. Create a "context" in which future expressions will be evaluated (which will be recreated on the cluster)
4. Create a "queue" that uses that context
5. Queue expressions which will be run at some future time on the cluster
6. Monitor progress, retrieve results, etc.



## Directories

I'm just going to assume your project is self contained and contains no references to absolute paths.  The directory that the calculations need are on a network share.  We can relax some of these requirements in future versions; talk to me.

## Credentials

The simplest way to set these is to set these globally, which can be done most simply by setting and forgetting.  Mine are:


```r
didewin::didewin_config_global(credentials="~/.smbcredentials",
                               home="~/net/home",
                               temp="~/net/temp",
                               cluster="fi--didemrchnb")
```

See `?didewin_config` for details here.

Hopefully some of these can be auto-detected at some point.

* `credentials`: The `~/.smbcredentials` file is due to the file system set up I have.  If you put just your username here you will be prompted interactively for your DIDE password.  You can supply a username/password list but that's not ideal because your password ends up in plain text and then in things like your `.Rhistory` file!  On windows this will default to your username.
* `home`: path to the mount point of your home share (not required for windows users)
* `temp`: path to the mount point of your home share (not required for windows users)
* `cluster`: name of the cluster to use.  The other option, and default, is `"fi--dideclusthn"`.

A minimal set up on windows where this can be automated is:

```r
didewin::didewin_config_global(cluster="fi--didemrchnb")
```

Once your credentials are set up, try logging in with:


```r
didewin::web_login()
```

which will return an error if login was unsuccessful and otherwise prints nothing.  Your credentials are sent to the server in the same way as with the web portal; base64 encoded (i.e., essentially plain text) over a secure https connection.

In general you will not need to run the `web_login` function above, but it's worth doing so first time to make sure that everything looks OK.

## Contexts

For detailed discussion on this, please see the [`context`](http://dide-tools.github.io/context) package, and especially its [vignette](http://dide-tools.github.io/context/vignettes/context.html).  Essentially "contexts" are recipes for reconstructing your local R environment on a remote computer that might be configured differently to yours.  So, things like

* installing packages (especially if you run non windows, or have 32/64 bit differences).
* sourcing R scripts to create functions and objects
* global and local environments
* collating variables local to an expression

Once all the above is set up, actually evaluating the R expression you want to run remotely is pretty straightforward.

So, suppose I have a script file `myfunctions.R` that contains simply the function definition:

```r
make_trees <- function(n, nspp) {
  lapply(seq_len(n), function(...) ape::rtree(nspp))
}
```

which is just going to create `n` trees each of which have `nspp`.  Perhaps this is some slow tree bootstrapping/mcmc proceedure :)




```r
root <- "contexts"
ctx <- context::context_save(packages="ape", sources="myfunctions.R", root=root)
```

This might take a second as part of the process involves downloading context itself so that it can be installed on the cluster.

The important bits here are:

* `packages`: a vector of packages to be loaded via `library` on the cluster
* `sources`: *relative paths* to files to be read via `source`
* `root`: the directory to store all the bits to accomplish this.

The directory `contexts` now contains files:


```r
dir(root)
```

```
## [1] "context_bootstrap" "context_runner"    "contexts"
## [4] "context_version"   "drat"
```

but in general you do not need to know or care what is going on in there (see the context package if you're curious).

## Queues


```r
obj <- didewin::queue_didewin(ctx)
```

If the above command does not throw an error, then you have successfully logged in.

This is a weird sort of object called an `R6` class.  It's a bit like a Python or Java class if you've come from those languages.


```r
obj
```

```
## <queue>
##   Public:
##     clone: function
##     config: didewin_config
##     context: context_handle
##     enqueue: function
##     enqueue_: function
##     initialize: function
##     load: function
##     login: function
##     root: contexts
##     set_cluster: function
##     set_context: function
##     submit: function
##     task_get: function
##     task_result: function
##     tasks_drop: function
##     tasks_list: function
##     tasks_status: function
##     tasks_times: function
##     workdir: /home/rich/net/home/didewin_demo
```

List tasks that we are running already:


```r
obj$tasks_list()
```

```
## character(0)
```

(note that this is only tasks that are being run via didewin/context, *not* all your tasks).

## Queue an expression

This is the bit that starts doing some actual work for you.  I want to run the `make_trees` function defined in `myfunctions.R`.

To make 5 trees of 10 species each:


```r
t <- obj$enqueue(make_trees(5, 10))
```

The returned object is another R6 object that can be used to query the task


```r
t
```

```
## <task>
##   Public:
##     clone: function
##     config: didewin_config
##     context: function
##     expr: function
##     handle: task_handle
##     id: 1b72130fdb8d49f147c32705c69e5ba9
##     initialize: function
##     log: function
##     result: function
##     root: contexts
##     status: function
##     times: function
##     wait: function
```


```r
t$status()
```

```
## [1] "PENDING"
```

```r
t$times()
```

```
##                            task_id           submitted started finished
## 1 1b72130fdb8d49f147c32705c69e5ba9 2015-11-26 15:27:00    <NA>     <NA>
##    waiting running idle
## 1 1.160853      NA   NA
```

Trying to get a task before it is done is an error:


```r
t$result()
```

```
## Error in task_result(self, self$id): task 1b72130fdb8d49f147c32705c69e5ba9 is unfetchable: PENDING
```

However, we can wait until it is done (here, waiting for up to 1000s)


```r
res <- t$wait(1000)
res[[1]]
```

```
##
## Phylogenetic tree with 10 tips and 9 internal nodes.
##
## Tip labels:
## 	t6, t2, t3, t8, t1, t10, ...
##
## Rooted; includes branch lengths.
```

Success!


```r
t$status()
```

```
## [1] "COMPLETE"
```

```r
t$times()
```

```
##                            task_id           submitted             started
## 1 1b72130fdb8d49f147c32705c69e5ba9 2015-11-26 15:27:00 2015-11-26 15:27:00
##              finished waiting running    idle
## 1 2015-11-26 15:27:10       0      10 1.66972
```

```r
t$result()[[1]]
```

```
##
## Phylogenetic tree with 10 tips and 9 internal nodes.
##
## Tip labels:
## 	t6, t2, t3, t8, t1, t10, ...
##
## Rooted; includes branch lengths.
```

The rather long running time above (10 s) is due to downloading and installing all the required packages.  This is a bit clearer in the log for that job (this function can be run even when a task is running still).


```r
t$log()
```

```
## [ bootstrap ]  Q:\didewin_demo\contexts
## [ lib       ]  Q:\didewin_demo\contexts/R/x86_64-w64-mingw32/3.2
## [ install   ]  context
##     also installing the dependencies 'curl', 'drat'
##
##     trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.2/curl_0.9.4.zip'
##     Content type 'application/zip' length 4384579 bytes (4.2 MB)
##     ==================================================
##     downloaded 4.2 MB
##
##     trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.2/drat_0.1.0.zip'
##     Content type 'application/zip' length 70331 bytes (68 KB)
##     ==================================================
##     downloaded 68 KB
##
##     package 'curl' successfully unpacked and MD5 sums checked
##     package 'drat' successfully unpacked and MD5 sums checked
##
##     The downloaded binary packages are in
##     	C:\Users\rfitzjoh\AppData\Local\Temp\Rtmpkp9OSs\downloaded_packages
##     installing the source package 'context'
##
##     * installing *source* package 'context' ...
##     ** R
##     ** inst
##     ** preparing package for lazy loading
##     ** help
##     *** installing help indices
##     ** building package indices
##     ** installing vignettes
##     ** testing if installed package can be loaded
##     *** arch - i386
##     *** arch - x64
##     * DONE (context)
##
##     The downloaded source packages are in
##     	'C:\Users\rfitzjoh\AppData\Local\Temp\Rtmpkp9OSs\downloaded_packages'
## [ done      ]
## [ init      ]  2015-11-26 15:27:08.518
## [ version   ]  0.0.2
## [ root      ]  Q:\didewin_demo\contexts
## [ task      ]  1b72130fdb8d49f147c32705c69e5ba9
## [ context   ]  ae37ce673914ec8221b76836d0c1321f
## [ lib       ]  Q:\didewin_demo\contexts/R/x86_64-w64-mingw32/3.2
## [ install   ]  ape
##     Installing package into 'Q:/didewin_demo/contexts/R/x86_64-w64-mingw32/3.2'
##     (as 'lib' is unspecified)
##     trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.2/ape_3.3.zip'
##     Content type 'application/zip' length 1399090 bytes (1.3 MB)
##     ==================================================
##     downloaded 1.3 MB
##
##     package 'ape' successfully unpacked and MD5 sums checked
##
##     The downloaded binary packages are in
##     	C:\Users\rfitzjoh\AppData\Local\Temp\Rtmpkp9OSs\downloaded_packages
## [ library   ]  ape
## [ namespace ]
## [ source    ]  myfunctions.R
## [ expr      ]  make_trees(5, 10)
## [ start     ]  2015-11-26 15:27:10.862
## [ result    ]  Q:\didewin_demo\contexts/task_results/1b72130fdb8d49f147c32705c69e5ba9
## [ end       ]  2015-11-26 15:27:10.877
```

Future expressions that use the same packages will be faster:


```r
t2 <- obj$enqueue(make_trees(1, 20))
res2 <- t2$wait(1000)
obj$tasks_times()[c("task_id", "submitted", "running")]
```

```
##                            task_id           submitted running
## 1 1b72130fdb8d49f147c32705c69e5ba9 2015-11-26 15:27:00      10
## 2 381520c40a9c293e7d6eea67b8d35ffd 2015-11-26 15:27:12       1
```

In the log you can see a quicker startup period

```r
t2$log()
```

```
## [ bootstrap ]  Q:\didewin_demo\contexts
## [ lib       ]  Q:\didewin_demo\contexts/R/x86_64-w64-mingw32/3.2
## [ ok        ]
## [ init      ]  2015-11-26 15:27:12.893
## [ version   ]  0.0.2
## [ root      ]  Q:\didewin_demo\contexts
## [ task      ]  381520c40a9c293e7d6eea67b8d35ffd
## [ context   ]  ae37ce673914ec8221b76836d0c1321f
## [ lib       ]  Q:\didewin_demo\contexts/R/x86_64-w64-mingw32/3.2
## [ library   ]  ape
## [ namespace ]
## [ source    ]  myfunctions.R
## [ expr      ]  make_trees(1, 20)
## [ start     ]  2015-11-26 15:27:13.096
## [ result    ]  Q:\didewin_demo\contexts/task_results/381520c40a9c293e7d6eea67b8d35ffd
## [ end       ]  2015-11-26 15:27:13.096
```

The task handles can be recreated from the queue


```r
obj$tasks_list()
```

```
## [1] "1b72130fdb8d49f147c32705c69e5ba9" "381520c40a9c293e7d6eea67b8d35ffd"
```

```r
obj$task_get(obj$tasks_list()[[1]])
```

```
## <task>
##   Public:
##     clone: function
##     config: didewin_config
##     context: function
##     expr: function
##     handle: task_handle
##     id: 1b72130fdb8d49f147c32705c69e5ba9
##     initialize: function
##     log: function
##     result: function
##     root: contexts
##     status: function
##     times: function
##     wait: function
```

The queue can be recreated in a fresh R session.  Unfortunately at the moment that's really ugly:

```r
ctx <- context:::context_handle(dir, root(file.path(root, "contexts")))
obj <- didewin::queue(ctx)
```

# Next steps

This is an experiment.  Nicer tools will be built on top of this; things like `lapply` for clusters.

Other directions I am thinking of:

* workflow support for where a very large number of jobs need to be submitted but throttled
* jobs that depend on other jobs
* run part of the queue locally at the same time it's running on the cluster, which could allow ad-hoc clustering.

# Installation

This package requires [context](https://github.com/dide-tools/context) so install that first.

```r
devtools::install_github(c(
  "richfitz/ids",
  "dide-tools/context",
  "richfitz/queuer",
  "dide-tools/didewin"))
```


# Setting up network shares

For all options you will need to be on the wired network or VPN I believe.

## Windows

This is done automatically.  `Q:` is your home directory and `T:` is the temporary directory.

## Linux

Full instructions [on the Ubuntu community wiki](https://help.ubuntu.com/community/MountWindowsSharesPermanently).

Install cifs-utils

```
sudo apt-get install cifs-utils
```

In your `/etc/fstab` file, add

```
//fi--san02/homes/<dide-username> <home-mount-point> cifs uid=<local-userid>,gid=<local-groupid>,credentials=/home/<local-username>/.smbcredentials,domain=DIDE,sec=ntlmssp,iocharset=utf8 0  0
//fi--didef2/tmp <tmp-mount-point> cifs uid=<local-userid>,gid=<local-groupid>,credentials=/home/<local-username>/.smbcredentials,domain=DIDE,sec=ntlmssp,iocharset=utf8 0  0
```

where:

- `<dide-username>` is your dide username without the `DIDE\` bit.
- `<local-username>` is your local username (i.e., `echo $USER`).
- `<local-userid>` is your local numeric user id (i.e. `id -u $USER`)
- `<local-groupid>` is your local numeric group id (i.e. `id -g $USER`)
- `<home-mount-point>` is where you want your DIDE home directory mounted
- `<tmp-mount-point>` is where you want the DIDE temporary directory mounted

**please back this file up before editing**.

So for example, I have:

```
//fi--san02/homes/rfitzjoh /home/rich/net/home cifs uid=1000,gid=1000,credentials=/home/rich/.smbcredentials,domain=DIDE,sec=ntlmssp,iocharset=utf8 0  0
//fi--didef2/tmp /home/rich/net/temp cifs uid=1000,gid=1000,credentials=/home/rich/.smbcredentials,domain=DIDE,sec=ntlmssp,iocharset=utf8 0  0
```

The file `.smbcredentials` contains

```
username=<dide-username>
password=<dide-password>
```

and set this to be chmod 600 for a modicum of security.

This set up is clearly insecure.  I believe if you omit the credentials line you can have the system prompt you for a password interactively, but I'm not sure how that works with automatic mounting.

Finally, run

```
mount -a
```

to mount all drives and with any luck it will all work and you don't have to do this until you get a new computer.

## Mac

Guidance welcomed from anyone who can get this reliably working.

# Compiled code

Packages that include compiled code represent a challenge, as every node on the cluster needs a working windows compiler toolchain and that's tricky to guarantee.  This is even worse if you need C++11 support as the current R/Windows/C++ toolchain does not support C++11, but there is experimental support we can try and use, but that requires rebuilding R itself (and all packages that it uses) so it's not a great solution.

Options:

* Install Rtools on the cluster and require recent R versions for jobs that require it.
* Compile the package on windows and add to `context`'s drat repository:
  - via R-win-builder (automatic submission via `devtools` but no automatic collection).
  - appveyor
  - a dedicated windows build machine

The last option is probably the best for us as it will scale the most easily.  However, it might be worth waiting to see what Gabor comes up with via [r-hub](https://github.com/r-hub) first.
