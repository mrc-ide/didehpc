knitr::opts_chunk$set(error = FALSE)
set.seed(1)
options(repos="https://cran.rstudio.com",
        didehpc.credentials = "~/.smbcredentials",
        didehpc.cluster = "fi--didemrchnb",
        context.drat=paste0("file://", normalizePath("~/Documents/Projects/epi/cluster/drat")))
