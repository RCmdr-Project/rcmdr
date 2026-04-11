package := Rcmdr
version := 2.12.2

R := $(wildcard pkg/R/*.R)
Rnw := $(wildcard pkg/vignettes/*.Rnw)
Rd := $(wildcard pkg/man/*.Rd)

Vignettes := $(patsubst pkg/vignettes/%.Rnw,pkg/inst/doc/%.pdf,$(Rnw))

# User targets
default: $(package)_$(version).tar.gz
	@echo $(package)_$(version).tar.gz ready

check: Rcmdr.Rcheck/00check.log
	@echo Checking $(package)_$(version).tar.gz

install: ~/R_LIBS/Rcmdr/NAMESPACE
	@echo $(package)_$(version).tar.gz installed

vignettes: $(Vignettes)

zip: $(package)_$(version).zip
	@echo $(package)_$(version).zip ready

# Other targets
Rcmdr.Rcheck/00check.log: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz

$(package)_$(version).tar.gz: $(R) $(Rd) $(Rnw) $(Vignettes) pkg/DESCRIPTION pkg/NAMESPACE pkg/.Rbuildignore
	R CMD build --compact-vignettes=both pkg

$(package)_$(version).zip: Rcmdr.Rcheck/00check.log
	cd Rcmdr.Rcheck; zip -r ../$(package)_$(version).zip Rcmdr

pkg/inst/doc/%.pdf: pkg/vignettes/%.pdf
	cp --force $< $@

pkg/vignettes/%.tex: pkg/vignettes/%.Rnw
	cd pkg/vignettes; R CMD Sweave $(subst pkg/vignettes/,,$<)

pkg/vignettes/%.pdf: pkg/vignettes/%.tex
	cd pkg/vignettes; latexmk $(subst pkg/vignettes/,,$<)

pkg/NAMESPACE: $(R)
	R -e "library('tcltk2'); roxygen2::roxygenize('pkg')"
	# Temporary patch until all Rd were migrated
	touch pkg/DESCRIPTION

~/R_LIBS/Rcmdr/NAMESPACE: $(package)_$(version).tar.gz
	@R CMD INSTALL $<
