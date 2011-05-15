# Generated automatically from Makefile.in by configure.
################################################################################
##
#  Makefile.in - Template Makefile for Luther Reform Prolog
#
#  Patric Hedlin.....Thu Jun 16 1994
#
#
# Compile time options:
#
# o ANSI_C	Used when the compiler supports ANSI C.
# o NDEBUG	Compile without debug code (will exclude the wam-debugger).
# o STATISTICS	Statistics of the number of calls, fails, and calls to 
#               builtin predicates.
# o NUMA	Support nonuniform memory architecture.
# o LOCK	Support mutually exclusive locking (parallel version).
# o SEMILOCK
#
# o CONSTR	Support constrained variables (i.e. suppension on variables)
# o UNBOUND	Support "unbounded" variable representation.
# o TIMESTAMP	Support timestamped variable representation. Extends each
#		variable with an extra 'create time' word.
#
# o COPY_GC	Include the copying garabage collector instead of the mark
#		and sweep ditto.
# o PARALLEL	Used when compiling the Reform parallel version.
#
#		 o SEMAPHORE
#		 o SWAP_BIND
#		 o SHARED_MEMORY
#		 o TIMESTAMP (conditionally)
#		 o ACTIVE_CONSUMER
#		 o REFORM
#
# o THREADED	Support threaded code emulation.
# o PREFETCH	Support (instruction) prefetching during emulation.
# o JUMP_CALL
#
# o VARDISPLAY
#
# o STAT_AWAIT
# o BOUNDED_Q
# o PARALLEL_BQ
# o TRAIL_ALL
#
# o DISABLE_HEAP_RECLAIM 
#

TOPDIR = /home/groucho/csd/jb/Reform/Luther

GMPDIR = $(TOPDIR)/gmp

BINDIR = $(TOPDIR)/bin

LIBDIR = $(TOPDIR)/Library


################################################################################
##
#   Define the proper compiler etc. 
#

M4 = m4

SED = sed

MAKE = make  -j 2


LEX = flex

LFLAGS =

LEX_FILE = lexer.l

LEX_SOURCE_FILE = $(LEX_FILE:.l=.c)

LEX_OBJECT_FILE = $(LEX_SOURCE_FILE:.c=.o)


YACC = bison -y

YFLAGS =

YACC_FILE = parser.y

YACC_SOURCE_FILE = $(YACC_FILE:.y=.c)

YACC_OBJECT_FILE = $(YACC_SOURCE_FILE:.c=.o)


TAGS = etags

TAGS_FILE = TAGS


LDLIBS  = -lgmp -lm  -lfl # -lthread # -lsocket
#LDLIBS  = -lgmp -lm  -lfl  -lthread -lposix4
#LDLIBS  = -lgmp -lm  -lsockets -lfl -lthread -lposix4
#LDLIBS  = -lgmp -lm  -lfl 

LDFLAGS = -L$(GMPDIR)

LDFILES = $(LUTHER_OBJECT_FILES) $(YACC_OBJECT_FILE) $(LEX_OBJECT_FILE)


################################################################################
##
#  Use the "DEFAULT_OPTIONS" variable to impose support/optionals not covered
# by the standard targets (e.g. using "DEFAULT_OPTIONS = -DCONSTR" will build
# luther with constrained/suspension variables, in all versions).
#

CONFIG_OPTIONS = -DHAVE_CONFIG_H

LUTHER_OPTIONS =

DEFAULT_OPTIONS = -DNUMA -DCOPY_GC

WARNING_OPTIONS =

LANGUAGE_OPTIONS = -DANSI_C

COMPILER_OPTIONS = -g -pipe

###

CC = gcc-2.6.3

CPPFLAGS = -I$(GMPDIR) $(CONFIG_OPTIONS) $(LUTHER_OPTIONS) $(DEFAULT_OPTIONS)

CFLAGS   = $(WARNING_OPTIONS) $(LANGUAGE_OPTIONS) $(COMPILER_OPTIONS)


################################################################################
##
#   
#

SYSTEM_HEADER_FILE = system.sun4.h

SYSTEM_SOURCE_FILE = system.sun4.c

SYSTEM_OBJECT_FILE = $(SYSTEM_SOURCE_FILE:.c=.o)


LUTHER_HEADER_FILES =	luther.h constants.h main.h \
			initial.h error.h inout.h char_table.h \
			engine.h unify.h \
			worker.h deref.h bind.h lock.h \
			expand_file_name.h load.h qload.h \
			time.h statistics.h \
			builtin.h inline.h math.h \
			assert.h array.h \
			bignum.h bnlib.h \
			term.h storage.h database.h \
			atom_table.h functor_table.h \
			instrdef.h instructions.h labelsort.h \
			display.h display_code.h \
			c.h think.h debug.h \
			unix.h signal.h event.h \
			semaphore.h parallel.h \
			event.c global_event.c \
			gc.h segment.h trail.h socket.h threads.h

# new-copy-gc.h


LUTHER_SOURCE_FILES =	main.c \
			initial.c error.c inout.c char_table.c \
			engine.c unify.c \
			expand_file_name.c load.c qload.c \
			time.c statistics.c \
			builtin.c inline.c \
			assert.c array.c \
			bignum.c bnlib.c \
			term.c storage.c database.c \
			atom_table.c functor_table.c \
			instrdef.c labelsort.c \
			display.c display_code.c \
			think.c debug.c \
			unix.c signal.c \
			semaphore.c parallel.c \
			gc.c segment.c \
			trail.c shift.c socket.c \
			new-copy-gc.c 

#			copy-gc-ali.c



LUTHER_OBJECT_FILES =	$(LUTHER_SOURCE_FILES:.c=.o)


LUTHER_MISCELLANEOUS =	Makefile.in



ALLOCA_SOURCE_FILE = alloca.c

ALLOCA_OBJECT_FILE = $(ALLOCA_SOURCE_FILE:.c=.o)


################################################################################
##
#   We define all transformations explicitly:
#
.c.o:
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

.l.c:
	/bin/rm -f $*.c
	$(LEX) -t $(LFLAGS) $< > $*.c

.y.c:
	$(YACC) -d $(YFLAGS) $<
	@mv y.tab.h $*.h
	@mv y.tab.c $*.c


################################################################################
##
#   Define dependencies in order to build various versions and objects.
#

def:	dbg

all:
	$(MAKE) trueclean
	$(MAKE) seq
	$(MAKE) clean
	$(MAKE) fseq
	$(MAKE) clean
	$(MAKE) par
	$(MAKE) clean
	$(MAKE) fpar
	$(MAKE) clean
	$(MAKE) spar
	$(MAKE) clean
	$(MAKE) debuglistner
	$(MAKE) clean

debuglistner: 
	$(CC) -g -c debuglistner.c
	$(LINK.c) -o $@ debuglistner.o $(LDLIBS)
	mv $@ $(BINDIR)/$@

dbg:
	$(MAKE) luther.seq
	mv luther.seq luther.$@

seq:
	$(MAKE) luther.seq "COMPILER_OPTIONS = -pipe -g"
	mv luther.seq $(BINDIR)/luther.$@


fseq:
	$(MAKE) luther.seq "LUTHER_OPTIONS = -DNDEBUG -DPREFETCH -DTHREADED" "COMPILER_OPTIONS = -pipe -O4"
	mv luther.seq $(BINDIR)/luther.$@

par:
	$(MAKE) luther.par "LUTHER_OPTIONS = -Dsun4 -DPARALLEL" "COMPILER_OPTIONS = -pipe -g"
	mv luther.par $(BINDIR)/luther.$@

fpar:
	$(MAKE) luther.par "LUTHER_OPTIONS = -Dsun4 -DNDEBUG -DPREFETCH -DTHREADED -DPARALLEL" "COMPILER_OPTIONS = -pipe -O4"
	mv luther.par $(BINDIR)/luther.$@

spar:
	$(MAKE) luther.par "LUTHER_OPTIONS = -Dsun4 -DPARALLEL -DTRAILSTAT" "COMPILER_OPTIONS = -pipe"
	mv luther.par $(BINDIR)/luther.$@


luther.seq: $(LDFILES)
	$(LINK.c) -o $@ $(LDFILES)  $(LDLIBS)
	chmod a+xr $@

luther.par: $(LDFILES) $(SYSTEM_OBJECT_FILE)
	$(LINK.c) -o $@ $(LDFILES) $(SYSTEM_OBJECT_FILE)  $(LDLIBS)
	chmod a+xr $@


$(LEX_OBJECT_FILE): $(LEX_FILE) $(YACC_FILE)

$(YACC_OBJECT_FILE): $(YACC_FILE)

$(SYSTEM_OBJECT_FILE): $(SYSTEM_HEADER_FILE)

$(LUTHER_OBJECT_FILES): $(LUTHER_HEADER_FILES)


m4.def: Makefile
	@echo $(LUTHER_OPTIONS) | $(SED) 's:-D\([a-zA-Z_0-9]*\):define(\1,1):g' | tr ' ' '\012' > $@


$(LEX_FILE): m4.def $(LEX_FILE).m4
	/bin/rm -f $@
	$(M4) m4.def $@.m4 > $@

$(YACC_FILE): m4.def $(YACC_FILE).m4
	/bin/rm -f $@
	$(M4) m4.def $@.m4 > $@


$(TAGS_FILE): $(LUTHER_HEADER_FILES) $(LUTHER_SOURCE_FILES) $(LEX_FILE:.l=.l.m4) $(YACC_FILE:.y=.y.m4)
	$(TAGS) -t $(LUTHER_HEADER_FILES) $(LUTHER_SOURCE_FILES)  \
	           $(LEX_FILE:.l=.l.m4) $(YACC_FILE:.y=.y.m4) -o $@

wc:
	echo `wc $(LUTHER_HEADER_FILES) $(LUTHER_SOURCE_FILES) $(LEX_FILE:.l=.l.m4) $(YACC_FILE:.y=.y.m4)`

clean: 
	/bin/rm -f m4.def
	/bin/rm -f $(LDFILES) $(SYSTEM_OBJECT_FILE) 

trueclean: clean
	/bin/rm -f debuglistner.o
	/bin/rm -f $(LEX_SOURCE_FILE)
	/bin/rm -f $(YACC_SOURCE_FILE)

realclean: trueclean
	/bin/rm -f TAGS
	/bin/rm -f luther.dbg core
