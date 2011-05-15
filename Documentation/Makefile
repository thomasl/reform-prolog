# File:    Makefile
# Author:  Johan Bevemyr
# Created: Thu Oct 24 13:19:35 MET 1991
# Purpose: Makefile for documentation

DATE= `date '+%y-%m-%d-%H.%M'`

emulator.dvi : emulator.texinfo
	tex emulator.texinfo
	texindex emulator.??
	tex emulator.texinfo

emulator.ps : emulator.dvi
	dvips -oemulator.ps emulator.dvi

clean : 
	/bin/rm -f emulator.log emulator.aux emulator.cp emulator.cps \
	emulator.fn emulator.fns emulator.ky emulator.kys emulator.pg \
	emulator.pgs emulator.toc emulator.tp emulator.tps emulator.vr \
	emulator.vrs


backup :
	tar cf - emulator.texinfo | compress > Backup-$(DATE).tar.Z

distclean : clean
	/bin/rm -f emulator.ps emulator.dvi
