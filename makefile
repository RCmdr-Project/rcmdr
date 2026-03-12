package := Rcmdr
version := 2.11.0
R := $(wildcard pkg/R/*.R)
Rd := $(wildcard pkg/man/*.Rd)
Vignettes  := $(wildcard pkg/vignettes/*)

# User targets
default: $(package)_$(version).tar.gz
	@echo $(package)_$(version).tar.gz ready

check: $(package).Rcheck
	@echo Checking $(package)_$(version).tar.gz
install: ~/R_LIBS/Rcmdr/NAMESPACE
	@echo $(package)_$(version).tar.gz installed

# Other targets
$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz

$(package)_$(version).tar.gz: $(R) $(Rd) $(Vignettes) pkg/DESCRIPTION pkg/NAMESPACE
	R CMD build pkg

pkg/NAMESPACE: $(R)
	R -e "library('tcltk2'); roxygen2::roxygenize('pkg')"
	# Temporary patch until all Rd were migrated
	touch pkg/DESCRIPTION

~/R_LIBS/Rcmdr/NAMESPACE: $(package)_$(version).tar.gz
	@R CMD INSTALL $<
