/*
 * display.c
 *
 * Johan Bevemyr.....Tue Jun  4 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include <math.h>

#include "luther.h"
#include "unify.h"
#include "bignum.h"
#include "display.h"


void display_term_fd(fd, term, w)
     int fd;
     TAGGED term;
     worker *w;
{
  if (term == (TAGGED) NULL)
    {
      FD_Print1(fd, "NULL");
    }
  else
    {
      DerefNLL(term, term);
    
      switch (TagOf(term)) {

      case HVA:
	{
#if defined(VARDISPLAY)
	  FD_Print2(fd, "_(h)%lx", term);
#else
	  FD_Print2(fd, "_%lx", term);
#endif
	}
	break;

#if defined(CONSTR)
      case CVA:
	{
	  FD_Print2(fd, "_c%lx", term);
	}
	break;
#endif /* CONSTR */      

      case SVA:
	{
#if defined(VARDISPLAY)
	  FD_Print2(fd, "_(s)%lx", term);
#else
	  FD_Print2(fd, "_%lx", term);
#endif
	}
	break;

      case NUM:
	{
	  FD_Print2(fd, "%ld", GetNumber(term));
	}
	break;

      case ATM:
	{
	  FD_Print2(fd, "%s", GetString(term, w));
	}
	break;

      case BOX:
	{
	  if (IsFLT(term))
	    {
	      double value = GetFloat(term);
	      double integral;
	      double fraction = modf(value, &integral);

	      if (integral == value)
		{
		  FD_Print2(fd, "%.1f", value);
		}
	      else
		{
		  FD_Print2(fd, "%#g", value);
		}
	    }
	  else
	    {
	      print_bignum_fd(fd, GetBignum(term), DECIMAL);
	    }
	}
	break;

      case GEN:
	{
	  GetMethod(print, term)(term, stdout, w);
	}
	break;

      case LST:
	{
	  FD_Print1(fd, "[");
	  display_term_fd(fd,Ref(GetCar(term)), w);
	  DerefNLL(term, Ref(GetCdr(term)));

	  while (IsLST(term))
	    {
	      FD_Print1(fd, ",");
	      display_term_fd(fd, Ref(GetCar(term)), w);
	      DerefNLL(term, Ref(GetCdr(term)));
	    }

	  if (term == atom_nil)
	    {
	      FD_Print1(fd, "]");
	    }
	  else
	    {
	      FD_Print1(fd, "|");
	      display_term_fd(fd, term, w);
	      FD_Print1(fd, "]");
	    }
	}
	break;
	    
      case STR:
	{
	  structure str = Struct(RemoveTag(term, STR));
	  int arity = ArityOf(str->functor);
	  int i;
	
	  FD_Print2(fd, "%s(", GetString(FunctorToAtom(str->functor), w));
	
	  for (i = 0; i != arity; i++)
	    {
	      display_term_fd(fd, Ref(GetUntArg(str, i)), w);
	      if (i != arity - 1)
		FD_Print2(fd, "%c", 44);
	    }
	  FD_Print1(fd, ")");
	}
	break;

      default:
	{
	  FD_Print2(fd, "Error in display_term: No such tag type %lu\n", TagOf(term));
	}
      }
    }
}


void display_term(fp, t, w)
    FILE *fp;
    TAGGED t;
    worker *w;
{
  if (t == (TAGGED) NULL)
    {
      PL_Print1(fp, "NULL");
    }
  else
    {
      register TAGGED term;

      DerefNLL(term, t);
    
      switch (TagOf(term)) {

      case HVA:
	{
#if defined(VARDISPLAY)
	  PL_Print2(fp, "_(h)%lx", term);
#else
	  PL_Print2(fp, "_%lx", term);
#endif
	}
	break;

#if defined(CONSTR)
      case CVA:
	{
	  PL_Print2(fp, "_c%lx", term);
	}
	break;
#endif /* CONSTR */      

      case SVA:
	{
#if defined(VARDISPLAY)
	  PL_Print2(fp, "_(s)%lx", term);
#else
	  PL_Print2(fp, "_%lx", term);
#endif
	}
	break;

      case NUM:
	{
	  PL_Print2(fp, "%ld", GetNumber(term));
	}
	break;

      case ATM:
	{
	  PL_Print2(fp, "%s", GetString(term, w));
	}
	break;

      case BOX:
	{
	  if (IsFLT(term))
	    {
	      double value = GetFloat(term);
	      double integral;
	      double fraction = modf(value, &integral);

	      if (integral == value)
		fprintf(fp, "%.1f", value);
	      else
		write_float(fp, term, 'g', 16, w);
	    }
	  else
	    {
	      print_bignum(fp, GetBignum(term), DECIMAL);
	    }
	}
	break;

      case GEN:
	{
	  GetMethod(print, term)(term, fp, w);
	}
	break;

      case LST:
	{
	  PL_Print1(fp, "[");
	  display_term(fp, Ref(GetCar(term)), w);
	  DerefNLL(term, Ref(GetCdr(term)));

	  while (IsLST(term))
	    {
	      PL_Print1(fp, ",");
	      display_term(fp, Ref(GetCar(term)), w);
	      DerefNLL(term, Ref(GetCdr(term)));
	    }

	  if (term == atom_nil)
	    {
	      PL_Print1(fp,"]");
	    }
	  else
	    {
	      PL_Print1(fp,"|");
	      display_term(fp,term,w);
	      PL_Print1(fp,"]");
	    }
	}
	break;
	    
      case STR:
	{
	  structure str = Struct(RemoveTag(term, STR));
	  int arity = ArityOf(str->functor);
	  int i;
	
	  PL_Print2(fp, "%s(", GetString(FunctorToAtom(str->functor), w));
	
	  for (i = 0; i != arity; i++)
	    {
	      display_term(fp, Ref(GetUntArg(str, i)), w);
	      if (i != arity - 1)
		PL_Print2(fp, "%c", 44);
	    }
	  PL_Print1(fp, ")");
	}
	break;

      default:
	{
	  PL_Print2(fp, "Error in display_term: No such tag type %lu\n", TagOf(term));
	}
      }
    }
  fflush(fp);
}


void write_term(fp,t,w)
    FILE *fp;
    TAGGED t;
    worker *w;
{
  register TAGGED term;

  if (t == (TAGGED) NULL)
    {
      PL_Print1(fp,"NULL");
    }
  else
    {
      DerefNLL(term, t);
    
      switch (TagOf(term)) {

      case HVA:
	{
#if defined(VARDISPLAY)
	  PL_Print2(fp, "_(h)%lx", term);
#else
	  PL_Print2(fp, "_%lx", term);
#endif
	}
	break;

#if defined(CONSTR)
      case CVA:
	{
	  PL_Print2(fp, "_c%lx", term);
	}
	break;
#endif /* CONSTR */      

      case SVA:
	{
#if defined(VARDISPLAY)
	  PL_Print2(fp, "_(s)%lx", term);
#else
	  PL_Print2(fp, "_%lx", term);
#endif
	}
	break;

      case NUM:
	{
	  PL_Print2(fp, "%d", GetNumber(term));
	}
	break;

      case ATM:
	{
	  if (not(AtomHasSpecial(term)))
	    {
	      PL_Print2(fp, "%s", GetString(term,w));
	    }
	  else if (AtomHasSquote(term))
	    {
	      char buf[2*MAXATOMLEN];
	      char i, *in = GetString(term,w), *out = buf;

	      while (i = *in++)
		{
		  if (i == '\'')
		    *out++ = '\'';
		  *out++ = i;
		}
	      *out = 0;
	    
	      PL_Print2(fp, "'%s'", buf);
	    }
	  else
	    {
	      PL_Print2(fp, "'%s'", GetString(term,w));
	    }
	}
	break;

      case BOX:
	{
	  if (IsFLT(term))
	    {
	      double value = GetFloat(term);
	      double integral;
	      double fraction = modf(value, &integral);

	      if (integral == value)
		fprintf(fp, "%.1f", value);
	      else
		write_float(fp, term, 'g', 16, w);
	    }
	  else
	    {
	      print_bignum(fp, GetBignum(term), DECIMAL);
	    }
	}
	break;

      case GEN:
	{
	  GetMethod(print, term)(term, fp, w);
	}
	break;

      case LST:
	{
	  PL_Print1(fp, "[");
	  write_term(fp, Ref(GetCar(term)), w);
	  DerefNLL(term, Ref(GetCdr(term)));

	  while (IsLST(term))
	    {
	      PL_Print1(fp, ",");
	      write_term(fp, Ref(GetCar(term)), w);
	      DerefNLL(term, Ref(GetCdr(term)));
	    }

	  if (term == atom_nil)
	    {
	      PL_Print1(fp, "]");
	    }
	  else
	    {
	      PL_Print1(fp, "|");
	      write_term(fp, term,w);
	      PL_Print1(fp, "]");
	    }
	}
	break;
	    
      case STR:
	{
	  structure str = Struct(RemoveTag(term, STR));
	  int arity = ArityOf(str->functor);
	  int i;

	  PL_Print2(fp, "%s(", GetString(FunctorToAtom(str->functor), w));
	
	  for (i = 0; i != arity; i++)
	    {
	      write_term(fp, Ref(GetUntArg(str, i)), w);
	      if (i != arity - 1)
		PL_Print2(fp, "%c", 44);
	    }
	  PL_Print1(fp, ")");
	}
	break;

      default:
	{
	  PL_Print2(fp, "Error in write_term: No such tag type %lu\n", TagOf(term));
	}
      }
    }
  fflush(fp);
}


/* Note: Using 'write_float' will guarantee the float to be printed as a valid float
 *       readable by the Prolog parser (reader).
 */
void write_float(fp, number, format, precision, w)
     FILE *fp;
     TAGGED  number;
     integer format;
     integer precision;
     worker *w;
{
  static char template[64];

  if (IsFLT(number))
    {
      integer i;

      double value = GetFloat(number);

      switch ((char)format) {

      case 'f':
      case 'e':
      case 'E':
	{
	  precision = max(precision, 1);
	}
	break;

      case 'g':
      case 'G':
	{
	  double integral;
	  double fraction = fabs(modf(value, &integral));

	  if (integral == 0.0 && fraction != 0.0)
	    {
	      if (value >= 0.0001)
		{
		  precision = max(precision, 1);
		}
	      else
		{
		  double scale;

		  for (scale = fraction * 10.0; scale < 1.0; scale *= 10.0);

		  scale = scale - floor(scale);

		  for (i = precision; i > 1; i--)
		    scale *= 10.0;

		  if (not(scale > 1.0))
		    {
		      format = (format == 'g' ? 'e' : 'E');

		      precision = 1;
		    }
		}
	    }
	}
	break;

      default:
	{
	  PL_Print2(fp, "Error: non supported format '%c'\n", format);
	}
      }

      template[0] = '%';
      template[1] = '.';

      sprintf(template + 2, "%d", precision);

      i = strlen(template);

      template[i] = (char)format;
      template[i + 1] = '\0';

      fprintf(fp, template, value);
    }
  else
    {
      PL_Print1(fp, "Error: write_float invoked with non float\n");
    }
}


/* NOTE: Differences in format directives are not supported yet.
 */
void write_radix(fp, number, format, radix, w)
     FILE *fp;
     TAGGED  number;
     integer format;
     integer radix;
     worker *w;
{
  assert(format == 'r' || format == 'R');

  DerefNLL(number, number);

  if (IsNUM(number))
    {
      NUM2BIG(w, number);
    }

  if (IsBIG(number))
    {
      print_bignum(fp, GetBignum(number), radix);
    }
  else
    {
      PL_Print1(fp, "Error: write_radix invoked with non integer\n");
    }
}


/* NOTE: Differences in format directives are not supported yet.
 */
void write_integer(fp, number, format, precision, w)
     FILE *fp;
     TAGGED  number;
     integer format;
     integer precision;
     worker *w;
{
  assert(format == 'd' || format == 'D');

  DerefNLL(number, number);

  if (IsNUM(number))
    {
      fprintf(fp, "%d", GetNumber(number));
    }
  else if (IsBIG(number))
    {
      print_bignum(fp, GetBignum(number), DECIMAL);
    }
  else
    {
      PL_Print1(fp, "Error: write_integer invoked with non integer\n");
    }
}


#if ! defined(NDEBUG)

static void dt(term, w)
     TAGGED term;
     worker *w;
{
  display_term(stdout, term, w);

  PL_Print1(stdout, "\n");

  fflush(stdout);
}

#endif /* not NDEBUG */
