# Generated automatically from Makefile.in by configure.
################################################################################
##
#  Makefile.in - Template Makefile for Luther Reform Prolog.
#
#  Patric Hedlin.....Fri Jun 17 1994
#
#

MAKE = make


config:
	configure
	@echo
	@echo 'Use "make {seq|par|all}" to build the desired objects.'
	@echo

###
##  Ensure that "lut" is executable.
#
script:
	@cat lut | grep -v "lut.in" > lut.tmp
	@mv lut.tmp lut
	@chmod a+xr lut

###
##  Build "configure"
#
configure: configure.in
	autoheader
	autoconf

###
##  Build various targets.
#
seq: script
	(cd gmp ; $(MAKE) libgmp.a)
	(cd Emulator ; $(MAKE) clean seq ; $(MAKE) clean fseq)

par: script
	(cd gmp ; $(MAKE) libgmp.a)
	(cd Emulator ; $(MAKE) clean debuglistner; $(MAKE) clean par ; \
	 $(MAKE) clean fpar ; $(MAKE) clean spar )

all: script
	(cd gmp ; $(MAKE) libgmp.a)
	(cd Emulator ; $(MAKE) all)


doc:
	@echo Not available yet.

man:
	@echo Not available yet.


clean:
	(cd gmp ; $(MAKE) clean)
	(cd Emulator ; $(MAKE) trueclean)

