RSCRIPT = Rscript --no-init-file
UPSTREAM = $(cat upstream)

all: refresh quickstart.md didehpc.md

%.Rmd: %.R
	${RSCRIPT} -e 'sowsear::sowsear("$<", output="$@")'

%.md: %.Rmd
	rm -f $@
	${RSCRIPT} -e 'knitr::knit("$<")'
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak
	rm -rf contexts

refresh:
	cp -p `cat UPSTREAM`/vignettes/src/*.R .
