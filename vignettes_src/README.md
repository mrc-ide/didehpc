## didehpc vignettes

Because running these vignettes requires things like cluster credentials and access to the shared filesystem it can't be run on CI. So we will create pre-compiled vignettes.

In order to run these, run the script

```
./scripts/build_vignettes
```

This will only run on Rich's desktop at the moment due to some paths hard coded but adapting it to run elsewhere would be fairly straightforward.

The process is we copy the real vignettes from `vignettes_src` onto a shared drive with cluster access, run the vignettes one by one, then rename the finished product from .md to .Rmd and copy that into the `vignettes` directory. This results in files that R will happily "run" on CI and in particular for making the pkgdown site.

Once this has run, then add and commit the `vignettes` directory.
