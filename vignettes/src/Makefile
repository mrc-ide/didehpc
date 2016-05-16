RSCRIPT = Rscript --no-init-file

all: quickstart.md didewin.md

%.Rmd: %.R
	${RSCRIPT} -e 'sowsear::sowsear("$<", output="$@")'

%.md: %.Rmd
	rm -f $@
	${RSCRIPT} -e 'knitr::knit("$<")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak
