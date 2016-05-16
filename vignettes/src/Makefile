RSCRIPT = Rscript --no-init-file

all: quickstart.md

quickstart.Rmd: quickstart.R
	${RSCRIPT} -e 'sowsear::sowsear("$<", output="$@")'

quickstart.md: quickstart.Rmd
	${RSCRIPT} -e 'knitr::knit("quickstart.Rmd")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak
