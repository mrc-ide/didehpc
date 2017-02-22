PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: install

test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

test_all:
	REMAKE_TEST_INSTALL_PACKAGES=true make test

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD check --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

check_all:
	TEST_INSTALL_PACKAGES=true make check

README_DIR=~/net/home/didehpc_demo
README.md: README.Rmd
	mkdir -p ${README_DIR}
	cp $< ${README_DIR}
	cd ${README_DIR} && Rscript -e "options(warnPartialMatchArgs=FALSE); knitr::knit('$<')"
	cp ${README_DIR}/$@ .
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak

clean:
	rm -f ${PACKAGE}_*.tar.gz
	rm -rf ${PACKAGE}.Rcheck

staticdocs:
	@mkdir -p inst/staticdocs
	Rscript -e "library(methods); staticdocs::build_site()"
	rm -f vignettes/*.html
	@rmdir inst/staticdocs
website: staticdocs
	./update_web.sh

vignettes: vignettes/quickstart.Rmd vignettes/didehpc.Rmd
	${RSCRIPT} -e "library(methods); devtools::build_vignettes()"

vignettes/%.Rmd: vignettes/src/%.R
	sh build_vignettes.sh

.PHONY: all test document install vignettes
