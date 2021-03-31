# Updating didehpc for new R versions

Three steps are required. This document uses the addition of `R 3.6.0` as an example, where the previous latest
install was `R 3.5.0`.

## Install the new version of R on all cluster nodes

* Look in `\\fi--didef3\Software\HPC\R`.
* Download the new R installer, and save it in the folder.
* Install it locally. Verify the executable folders created are of this form:
  * `C:\Program Files\R\R-3.6.0\bin\i386\Rscript.exe`
  * `C:\Program Files\R\R-3.6.0\bin\x64\Rscript.exe`
* Copy the most recent `setr32_3_5_0.bat` and `setr64_3_5_0.bat` to new filenames matching the new version.
* Edit each one, so that the directories match the above - for example:
```
set path=%path%;C:\Program Files\R\R-3.6.0\bin\x64
```
* Copy the most recent install file (eg, `install_r_3_5.0.bat`) to the new version name (`install_r_3_6_0.bat`).
* Edit it, and change the version variable to match the new version:
```
set R_VERSION=3.6.0
```
* Test the install manually on a single node by remote desktop.
* If all seems well, use HPC Cluster Manager to install it on the nodes. I recommend no more than 30 at a time.
* Update `update_r_all.bat` - add the new install batch file to the list, which gets used when installing a new node.

## Update the package

* On a new branch, update `R/config.R`
* Update this block of code in the function `rtools_versions`, which associates the right version of Rtools with the 
right version of R. Verify this is the correct version of Rtools [here](https://cran.r-project.org/bin/windows/Rtools/).
```r
  ret <- switch(r_version_2,
                "3.2" = list(path = "Rtools33", gcc = "gcc-4.6.3"),
                "3.3" = list(path = "Rtools33", gcc = "gcc-4.6.3"),
                "3.4" = list(path = "Rtools34", gcc = mingw),
                "3.5" = list(path = "Rtools34", gcc = mingw),
                "3.6" = list(path = "Rtools34", gcc = mingw),
                stop("Get Rich to upgrade Rtools"))
```
* Locate the `r_versions` function and update the list of valid versions with the new one. This should match the 
filenames of the new `setr32` and `setr64` batch files created earlier.
```r
r_versions <- function() {
  v <- c("3.2.2", "3.2.4", "3.3.1", "3.3.2", "3.4.0", "3.4.2", "3.4.4",
         "3.5.0", "3.6.0")
```
* Update the minor version in `DESCRIPTION`, and add an entry in `NEWS.md`.
* Commit changes on the branch - but wait for last step before PR / merging.

## Update the Build Server

Follow [the instructions on `buildr` itself](https://github.com/mrc-ide/buildr/blob/master/setup.md) to install and configure the new version on the build server.

## Finalising

* Make a pull Request for the new version of didehpc, request a review, and await merging.
* After merging, `drat` will need updating. 
* Inform `#cluster` channel on slack.
