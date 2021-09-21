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

check:
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all:
	${RSCRIPT} -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-manual'))"

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
inst/web/install: inst/scripts/install.R
	cp $< $@
website: staticdocs inst/web/install
	./update_web.sh

vignettes: vignettes/quickstart.Rmd vignettes/didehpc.Rmd vignettes/workers.Rmd vignettes/orderly.Rmd
	${RSCRIPT} -e "library(methods); devtools::build_vignettes(dependencies=FALSE)"

# vignettes/%.Rmd: vignettes/src/%.R
# 	sh build_vignettes.sh

common_lib:
	./inst/scripts/build_common_lib.R

.PHONY: all test document install vignettes website common_lib
