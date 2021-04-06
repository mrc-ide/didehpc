RSCRIPT = Rscript --no-init-file
UPSTREAM = $(cat upstream)

all: quickstart.md didehpc.md troubleshooting.md

%.md: %.Rmd
	rm -f $@
	${RSCRIPT} -e 'knitr::knit("$<")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak
