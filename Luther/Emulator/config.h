/* Emulator/config.h.  Generated automatically by configure.  */
/* Emulator/config.h.in.  Generated automatically from configure.in by autoheader.  */

/* Define if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
/* #undef _ALL_SOURCE */
#endif

/* Define if using alloca.c.  */
/* #undef C_ALLOCA */

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
/* #undef CRAY_STACKSEG_END */

/* Define if you have alloca, as a function or macro.  */
#define HAVE_ALLOCA 1

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
#define HAVE_ALLOCA_H 1

/* Define if you have a working `mmap' system call.  */
#define HAVE_MMAP 1

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown
 */
/* #undef STACK_DIRECTION */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if lex declares yytext as a char * by default, not a char[].  */
#define YYTEXT_POINTER 1

/*   If our test program "config.c" was successful in generating the
 * malloc_base.h file, HAVE_MALLOC_BASE_H will indicate this.
 */
#define HAVE_MALLOC_BASE_H

/* The number of bytes in a double.  */
#define SIZEOF_DOUBLE 8

/* The number of bytes in a unsigned long int.  */
#define SIZEOF_UNSIGNED_LONG_INT 4

/* Define if you have the cbrt function.  */
#define HAVE_CBRT 1

/* Define if you have the ftruncate function.  */
#define HAVE_FTRUNCATE 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getrusage function.  */
/* #undef HAVE_GETRUSAGE */

/* Define if you have the random function.  */
/* #undef HAVE_RANDOM */

/* Define if you have the shmget function.  */
#define HAVE_SHMGET 1

/* Define if you have the sigvec function.  */
/* #undef HAVE_SIGVEC */

/* Define if you have the stat function.  */
#define HAVE_STAT 1

/* Define if you have the statvfs function.  */
#define HAVE_STATVFS 1

/* Define if you have the times function.  */
#define HAVE_TIMES 1

/* Define if you have the <stdlib.h> header file.  */
#define HAVE_STDLIB_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the m library (-lm).  */
#define HAVE_LIBM 1

/* Define if you have the socket library (-lsocket).  */
#define HAVE_LIBSOCKET 1
