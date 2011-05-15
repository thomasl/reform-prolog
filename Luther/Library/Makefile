# Generated automatically from Makefile.in by configure.
################################################################################
##
#  Makefile.in - Makefile to build runtime library for Luther Reform Prolog.
#
#  Patric Hedlin.....Mon Jun 20 1994
#
#

.SUFFIXES: .wam .pl


TOPDIR = /home/groucho/csd/jb/Reform/Luther

LIBDIR = $(TOPDIR)/Library


SED = sed

MAKE = make

COMPILER =


################################################################################
##
#

LIBRARY_SOURCE_FILES =	boot.pl inline.pl builtin.pl \
			load.pl absolute_file_name.pl \
			inout.pl modules.pl consult.pl \
			listing.pl \
			expand_term.pl \
			read.pl read_term.pl \
			write.pl format.pl \
			tokenizer.pl current_op.pl \
			exception.pl freeze.pl \
			if.pl not.pl setof.pl sorts.pl unix.pl \
			interpret4.pl \
			error.pl \
			tree.pl shell.pl


LIBRARY_TARGET_FILES = $(LIBRARY_SOURCE_FILES:.pl=.wam)


LOAD_FILE = load.tmp


all:
	rm -f $(LOAD_FILE)
	$(MAKE) wam
	cat $(LOAD_FILE) | $(COMPILER)
	rm -f $(LOAD_FILE)


wam: $(LIBRARY_TARGET_FILES)


clean:
	rm -f $(LIBRARY_TARGET_FILES) $(LOAD_FILE)


.pl.wam:	
	echo "rcompile($*, [-analyse, -verbose, output_separate])." >> $(LOAD_FILE)
