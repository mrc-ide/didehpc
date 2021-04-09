all: didehpc.md packages.md quickstart.md troubleshooting.md workers.md

%.md: %.Rmd
	rm -f $@
	Rscript -e 'knitr::knit("$<")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak
