/*
 * expand_file_name.c
 *
 * Johan Bevemyr.....Fri Jun  7 1991
 *
 *
 * This file is stolen from akl.
 */ 

#include "luther.h"
#include "initial.h"
#include "engine.h"
#include "unify.h"
#include "expand_file_name.h"


#if ! defined(unix)
BOOL expand_file_name(name, target)
     char *name;
     char *target;
{
  strcpy(target,name);
  return TRUE;
}

#else /* defined(unix) */

#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

    
static char cwd[MAXPATHLEN+1];

static int mygetpwnam(in, out)
     char *in, *out;
{
  char buf[MAXPATHLEN+27];
  FILE *s;

  strcpy(buf, "exec csh -f -c echo\\ \\~");
  strcat(buf, in);
  if (0 > (long)(s=(FILE *)popen(buf, "r")) || !fgets(out, MAXPATHLEN, s))
    return 0;
  out[strlen(out)-1] = 0;
  pclose(s);
  return 1;
}

BOOL expand_file_name(name, target)
     char *name;
     char *target;
{
  register char *src, *dest, *temp;

#if defined(HAVE_GETCWD)
  getcwd(cwd,MAXPATHLEN+1);
#else
  getwd(cwd);
#endif

  src = name, dest = target;
  if (src[0] != '/' && src[0] != '~')
    {
      strcpy(dest,cwd);
      dest += strlen(dest);
      *dest++ = '/';
    }
				/* phase 1: expand ~...  and $... */

 state0:			/* prev char is '/' or b.o.l. */
  switch (*dest++ = *src++)
    {
    case 0:
      src = dest = target;
      goto state20;
    case '/':
      goto state0;
    case '~':
      temp = dest;
      goto stateusr;
    case '$':
      temp = dest;
      goto stateenv;
    }

 state1:			/* inside file name component */
  switch (*dest++ = *src++)
    {
    case 0:
      src = dest = target;
      goto state20;
    case '/':
      goto state0;
    default:
      goto state1;
    }

 stateusr:			/* inside ~... component */
  switch (*dest++ = *src++)
    {
    case 0:
    case '/':
      --src, --dest, dest[0] = (char)0;
      if (temp == dest)
	{
	  --dest, dest[0] = (char)0;
	  if (!(temp = getenv("HOME")))
	    BuiltinError("expand_file_name", " - unable to find home directory","");

	  strcat(dest,temp);
	  dest += strlen(dest);
	}
      else
	{
	  struct passwd *pw;

	  dest = temp-1, dest[0] = (char)0;

	  if (!mygetpwnam(temp,dest))
	    BuiltinError("expand_file_name", " - unable to locate user - ", temp);
	  dest += strlen(dest);
	}	
      goto state0;
    default:
      goto stateusr;
    }

 stateenv:			/* inside $... component */
  switch (*dest++ = *src++)
    {
    case 0:
    case '/':
      --src, --dest, dest[0] = (char)0;
      if (temp == dest)
	dest[0] = '$';
      else
	{
	  char *var = temp;

	  dest = temp-1, dest[0] = (char)0;
	  if (!(temp = getenv(temp)))
	    BuiltinError("expand_file_name", " - undefined variable - ", var);

	  strcat(dest,temp);
	  dest += strlen(dest);
	}
      goto state0;
    default:
      goto stateenv;
    }

				/* phase 2: contract "." and ".." components */

 state10:			/* prev char is '/' */
  switch (*dest++ = *src++)
    {
    case 0:
      if (dest-2 > target)
	dest[-2] = 0;
      return TRUE;
    case '/':
#if defined(aegis)
      if (dest-3 < target || dest[-3] == '/')
	{
	  dest = target, *dest++ = '/', *dest++ = '/';
	  goto state10;
	}
#endif
      dest = target, *dest++ = '/';
      goto state10;
    case '.':
      if (src[0] == '/' || src[0] == (char)0)
	{
	  dest -= 2;
	  if (dest-1 <= target)
	    dest++;
	}
      else if (src[0] == '.' && (src[1] == '/' || src[1] == (char)0))
	{
	  src++, dest -= 2;
	  if (dest-1 <= target)
	    dest++;
	  while (--dest, dest[0] != '/')
	    ;
	  if (dest-1 <= target)
	    dest++;
	}
      if (dest[-1] == '/')
	goto state10;
    }

 state20:
  switch (*dest++ = *src++)
    {
    case 0:
      return TRUE;
    case '/':
      goto state10;
    default:
      goto state20;
    }
}

#endif /* unix */


/*******************************************************************************
 *
 * expand_file_name(+FILEPATH,?FILE,?DIRECTORY)
 *	 FILE is unified with the file name part of FILENAME after FILENAME
 *	have been expanded. DIRECTIRY is unified with the directory part of
 *	the expanded path.
 *    
 *	The predicate succeeds even if the file does not exist.
 *
 * Example:
 *	expand_file_name('~jb/.lutrc.pl','.lutrc.pl','/home/groucho/csd/jb').
 *
 */

BOOL luther_expand_file_name(Arg)
     Argdecl;
{
  TAGGED path, file, dir;
  char a_path[MAXPATHLEN+1];
  char a_dir[MAXPATHLEN+1];

  DerefNLL(path, Xw(0));
  DerefNLL(file, Xw(1));
  DerefNLL(dir,  Xw(2));

  if (not(IsATM(path)))
    return FALSE;

  if (not(expand_file_name(GetString(path,w), a_path)))
    {
      return FALSE;
    }
  else
    {
      int i, last_slash;

      for (i = 0, last_slash = -1; a_path[i] != '\0'; i++)
	{
	  if (a_path[i] == '/')
	    {
	      last_slash = i;
	    }
	}

      if (last_slash == -1)
	{
	  strcpy(a_dir, ".");
	}
      else
	{
	  strncpy(a_dir,a_path,last_slash);
	  a_dir[last_slash] = '\0';
	}
      return unify(store_atom(a_path,w), file,w) && 
	     unify(store_atom(a_dir, w), dir, w);
    }
}
#if 0
/*
 * expand_file_name.c
 *
 * Johan Bevemyr.....Fri Jun  7 1991
 *
 *
 * This file is stolen from akl.
 */ 

#include "luther.h"
#include "initial.h"
#include "engine.h"
#include "unify.h"
#include "expand_file_name.h"


#if ! defined(unix)
BOOL expand_file_name(name, target)
     char *name;
     char *target;
{
  strcpy(target,name);
  return TRUE;
}

#else /* defined(unix) */

#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

    
static char cwd[MAXPATHLEN+1];


BOOL expand_file_name(name, target)
     char *name;
     char *target;
{
  register char *src, *dest, *temp;

#if defined(HAVE_GETCWD)
  getcwd(cwd,MAXPATHLEN+1);
#else
  getwd(cwd);
#endif

  src = name, dest = target;
  if (src[0] != '/' && src[0] != '~')
    {
      strcpy(dest,cwd);
      dest += strlen(dest);
      *dest++ = '/';
    }

				/* phase 1: expand ~...  and $... */

 state0:			/* prev char is '/' or b.o.l. */
  switch (*dest++ = *src++)
    {
    case 0:
      src = dest = target;
      goto state20;
    case '/':
      goto state0;
    case '~':
      temp = dest;
      goto stateusr;
    case '$':
      temp = dest;
      goto stateenv;
    }

 state1:			/* inside file name component */
  switch (*dest++ = *src++)
    {
    case 0:
      src = dest = target;
      goto state20;
    case '/':
      goto state0;
    default:
      goto state1;
    }

 stateusr:			/* inside ~... component */
  switch (*dest++ = *src++)
    {
    case 0:
    case '/':
      --src, --dest, dest[0] = (char)0;
      if (temp == dest)
	{
	  --dest, dest[0] = (char)0;
	  if (!(temp = getenv("HOME")))
	    BuiltinError("expand_file_name", " - unable to find home directory","");

	  strcat(dest,temp);
	  dest += strlen(dest);
	}
      else
	{
	  struct passwd *pw;

	  dest = temp-1, dest[0] = (char)0;
	  if (!(pw = getpwnam(temp)))
	    BuiltinError("expand_file_name", " - unable to locate user - ", temp);

	  strcat(dest,(char *)pw->pw_dir);
	  dest += strlen(dest);
	}	
      goto state0;
    default:
      goto stateusr;
    }

 stateenv:			/* inside $... component */
  switch (*dest++ = *src++)
    {
    case 0:
    case '/':
      --src, --dest, dest[0] = (char)0;
      if (temp == dest)
	dest[0] = '$';
      else
	{
	  char *var = temp;

	  dest = temp-1, dest[0] = (char)0;
	  if (!(temp = getenv(temp)))
	    BuiltinError("expand_file_name", " - undefined variable - ", var);

	  strcat(dest,temp);
	  dest += strlen(dest);
	}
      goto state0;
    default:
      goto stateenv;
    }

				/* phase 2: contract "." and ".." components */

 state10:			/* prev char is '/' */
  switch (*dest++ = *src++)
    {
    case 0:
      if (dest-2 > target)
	dest[-2] = 0;
      return TRUE;
    case '/':
#if defined(aegis)
      if (dest-3 < target || dest[-3] == '/')
	{
	  dest = target, *dest++ = '/', *dest++ = '/';
	  goto state10;
	}
#endif
      dest = target, *dest++ = '/';
      goto state10;
    case '.':
      if (src[0] == '/' || src[0] == (char)0)
	{
	  dest -= 2;
	  if (dest-1 <= target)
	    dest++;
	}
      else if (src[0] == '.' && (src[1] == '/' || src[1] == (char)0))
	{
	  src++, dest -= 2;
	  if (dest-1 <= target)
	    dest++;
	  while (--dest, dest[0] != '/')
	    ;
	  if (dest-1 <= target)
	    dest++;
	}
      if (dest[-1] == '/')
	goto state10;
    }

 state20:
  switch (*dest++ = *src++)
    {
    case 0:
      return TRUE;
    case '/':
      goto state10;
    default:
      goto state20;
    }
}

#endif /* unix */


/*******************************************************************************
 *
 * expand_file_name(+FILEPATH,?FILE,?DIRECTORY)
 *	 FILE is unified with the file name part of FILENAME after FILENAME
 *	have been expanded. DIRECTIRY is unified with the directory part of
 *	the expanded path.
 *    
 *	The predicate succeeds even if the file does not exist.
 *
 * Example:
 *	expand_file_name('~jb/.lutrc.pl','.lutrc.pl','/home/groucho/csd/jb').
 *
 */

BOOL luther_expand_file_name(Arg)
     Argdecl;
{
  TAGGED path, file, dir;
  char a_path[MAXPATHLEN+1];
  char a_dir[MAXPATHLEN+1];

  DerefNLL(path, Xw(0));
  DerefNLL(file, Xw(1));
  DerefNLL(dir,  Xw(2));

  if (not(IsATM(path)))
    return FALSE;

  if (not(expand_file_name(GetString(path,w), a_path)))
    {
      return FALSE;
    }
  else
    {
      int i, last_slash;

      for (i = 0, last_slash = -1; a_path[i] != '\0'; i++)
	{
	  if (a_path[i] == '/')
	    {
	      last_slash = i;
	    }
	}

      if (last_slash == -1)
	{
	  strcpy(a_dir, ".");
	}
      else
	{
	  strncpy(a_dir,a_path,last_slash);
	  a_dir[last_slash] = '\0';
	}
      return unify(store_atom(a_path,w), file,w) && 
	     unify(store_atom(a_dir, w), dir, w);
    }
}
#endif 
