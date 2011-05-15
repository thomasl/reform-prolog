/*    File:	 copy.c  (~jb/Reform/Luther/Emulator/copy.c)
 *    Author:	 Johan Bevemyr
 *    Created:	 Thu Dec  1 11:16:09 1994
 *    Purpose:   
 */ 

#define IsCopied(T)       IsMarked(*TagToPointer(T))
#define GetNewLocation(T) Tagify(TagToPointer(*TagToPointer(T)),TagOf(T))

static TAGGED copy_variable PROTO((TAGGED, copy_rec *));
static TAGGED scan PROTO((TAGGED, TAGGED, copy_rec *));
TAGGED *copy_term PROTO((TAGGED,copy_rec *));
TAGGED *copy_hva PROTO((TAGGED,copy_rec *));
TAGGED *copy_cva PROTO((TAGGED,copy_rec *));
TAGGED *copy_box PROTO((TAGGED,copy_rec *));
TAGGED *copy_list PROTO((TAGGED,copy_rec *));
TAGGED *copy_struct PROTO((TAGGED,copy_rec *));

static TAGGED copy_variable(start, copy_state)
     TAGGED start;
     copy_rec *copy_state;
{
  TAGGED *result;
  TAGGED *next, tag, *old;

  tag = TagOf(start);
  old = TagToPointer(start);

  if(IsCopied(*old))
    {
      return Tagify(GetNewLocation(*old), tag);
    }
  else if(IsHeapTerm(start) || IsHeapBox(start))
    {
      /* First we copy the root */

      next = copy_term(start, copy_state);

      result = scan(next, tag, copy_state);
    
      return Tagify(result,tag);
    }
  else
    {
      Error("copy-gc-ali: nothing had to be copied");
      return start;
    }
}

/* We assume that all internal live variables have been marked
 */

static TAGGED scan(next, tag, copy_state)
     TAGGED *next, tag;
     copy_rec *copy_state;
{
  TAGGED *prev;
  BOOL last;
  BOOL internal;

  prev = NULL;              /* dummy start value */

 forward:
  if(IsMarked(*next))
    last = TRUE;
  else
    last = FALSE;

  if(IsMarkedF(*next))
    internal = TRUE;
  else
    internal = FALSE;

  switch(TagOf(*next))
    {
    case HVA:
      if(IsCopied(*next))
	{
	  *next = Tagify(GetNewLocation(*next),HVA);
	  goto backward;
	}
      else
	{
	  TAGGED oldnext = *next;

	  *next = Tagify(prev,tag);
	  tag = TagOf(oldnext);
	  prev = next;

	  if(last == TRUE)
	    Mark(*prev);

	  if(internal == TRUE)
	    MarkF(*prev);

	  next = copy_hva(old_next,copy_state);

	  goto forward;
	}
      break;
		
#ifdef CONSTR
    case CVA:
#endif /* CONSTR */
    case SVA:
      FatalError("Stack reference from heap");
#ifdef UNBOUND
    case UVA:
#endif 
    case NUM:
      goto backward;

    case BXN:
      if(IsHeapBox(*next))
	{
	  goto backward;
	}
      else if(IsCopied(*next))
	{
	  *next = Tagify(GetNewLocation(*next), BXN);
	  goto backward;
	}
      else
	{
	  *next = Tagify(copy_term(*next, copy_state), BXN);
	  goto backward;
	}

    case ATM:
      goto backward;

    case LST:
      if(IsCopied(*next))
	{
	  *next = Tagify(GetNewLocation(*next), LST);
	  goto backward;
	}
      else
	{
	  TAGGED oldnext;

	  oldnext = *next;
	  *next = Tagify(prev,tag);
	  tag = TagOf(oldnext);
	  prev = next;

	  if(last == TRUE)
	    Mark(*prev);
	  
	  if(internal == TRUE)
	    MarkF(*prev);

	  next = copy_list(oldnext, copy_state);

	  goto forward;
	}
	      
    case STR:
      if(IsCopied(*next))
	{
	  *next = Tagify(GetNewLocation(*next), STR);
	  goto backward;
	}
      else
	{
	  TAGGED oldnext;

	  oldnext = *next;
	  *next = Tagify(prev,tag);
	  tag = TagOf(oldnext);
	  prev = next;

	  if(last == TRUE)
	    Mark(*prev);

	  if(internal==TRUE)
	    MarkF(*prev);

	  next = copy_struct(oldnext, copy_state);

	  goto forward;
	}
    case GEN:
    default:
      FatalError("unknown tag in gc-scan");
    }

 backward:
  if(last == FALSE)
    {
      next = next - VARSIZE;
      goto forward;
    }
  else
    {
      UnMark(*next);

      if(internal==TRUE)
	{
	  UnMarkF(*next);

	  while(!(IsMarkedF(*next)))
	    next += VARSIZE;

	  UnMarkF(*next);
	}

      if(prev == NULL)		/* done */
	{
	  return Tagify(next,tag);
	}
      else if(IsMarked(*prev))
	{
	  TAGGED oldprev;

	  *prev = Tagify(next,tag);
	  next = prev;
	  prev = TagToPointer(oldprev);
	  tag = TagOf(oldprev);

	  goto backward;
	}
      else
	{
	  TAGGED oldprev;

	  *prev = Tagify(next,tag);
	  next = prev-VARSIZE;
	  prev = TagToPointer(oldprev);
	  tag = TagOf(oldprev);

	  goto forward;
	}
    }
}

TAGGED *copy_term(term, copy_state)
     TAGGED term;
     copy_rec *copy_state;
{
  switch(TagOf(term))
    {
    case HVA:
      return copy_hva(term, copy_state);

#ifdef CONSTR
    case CVA:
      return copy_cva(term, copy_state);
#endif  /* CONSTR */

    case SVA:
      Error("stack variable in heap");
      break;

    case NUM:
      Error("copy_term: trying to copy a non-heap term");
      break;

    case BXN:
      return copy_box(term, copy_state);

    case ATM:
      Error("copy_term: trying to copy a non-heap term");
      break;

    case LST:
      return copy_list(term, copy_state);

    case STR:
      return copy_struct(term, copy_state);

    case GEN:
      Error("copy_term: trying to copy a non-heap term");
      break;
    }
}

TAGGED *copy_hva(term, copy_state)
     TAGGED term;
     copy_rec *copy_state;
{
  TAGGED *new;
  TAGGED *old;

  new = copy_state->new_top;
  old = TagToPointer(term);

  /* calculate the distance to the head of the structure */

  if (IsOldStruct(old)) 
    {
      int dist, i;

      /* find head of struct */
      for(dist=0 ; IsMarkedF(*old) ; old-=VARSIZE, dist+=VARSIZE);

      
      /* copy structure */
      new[0] = old[0];
      old[0] = CopyMark(&(new[0]));

      if(VARSIZE==2)
	new[1] = old[1];

      for(i=VARSIZE ; IsMarkedF(old[i]) ; i+=VARSIZE, old+=VARSIZE)
	{
	  new[i] = RemoveMarkF(old[i]);
	  old[i] = CopyMark(&(new[i]));

	  if(VARSIZE==2)
	    new[i+1] = old[i+1];
	}

      if(dist == 0)
	{
	  Mark(new[0]);
	  new += i;
	  return (new-VARSIZE)
	}
      else
	{
	  Mark(new[0]);
	  MarkF(new[0]);
	  MarkF(new[dist]);
	  new += i;
	  return (new-VARSIZE);
	}
    }
  else
    {
      new[0] = old[0];
      old[0] = CopyMark(new);
      
      if(VARSIZE==2)
	new[1] = old[1];
      
      Mark(new[0]);
      new += VARSIZE;
      
      return (new-VARSIZE);
    }      
}


TAGGED *copy_cva(term, copy_state)
     TAGGED term;
     copy_rec *copy_state;
{
  TAGGED *new;
  TAGGED *old;

  new = copy_state->new_top;
  old = TagToPointer(term);
	
  new[0] = old[0];	            
  old[0] = CopyMark(&(new[0])); 
  new[VARSIZE] = RemoveMarkF(old[VARSIZE]);
  old[VARSIZE] = CopyMark(&(new[VARSIZE]));
	
  if(VARSIZE==2)
    {
      new[1] = old[1];
      new[3] = old[3];
    }

  Mark(new[0]);
  new += 2*VARSIZE;

  return (new-VARSIZE);
}


TAGGED *copy_box(term, copy_state)
     TAGGED term;
     copy_rec *copy_state;
{
  TAGGED *new, *ret;
  TAGGED *old;
  register s32 i, size;

  ret = new = copy_state->new_top;

  old = TagToPointer(term);
  size = GetBoxSize(*old);
	
  for(i=0 ; i < size*VARSIZE ; i++) /* copy term */
    {
      new[i] = old[i];
    }

  old[0] = CopyMark(&(new[0])); /* update oldspace */

  Mark(new[0]);
  new += size*VARSIZE;

  return ret;
}

TAGGED *copy_list(term, copy_state)
     TAGGED term;
     copy_rec *copy_state;
{
  TAGGED *new;
  TAGGED *old;

  new = copy_state->new_top;
  old = TagToPointer(term);

  new[0] = old[0];		/* copy term       */
  old[0] = CopyMark(&(new[0]));	/* update oldspace */
  new[VARSIZE] = RemoveMarkF(old[VARSIZE]); /* copy term       */
  old[VARSIZE] = CopyMark(&(new[VARSIZE])); /* update oldspace */
	
  if(VARSIZE==2)
    {
      new[1] = old[1];
      new[3] = old[3];
    }

  Mark(new[0]);			/* mark first cell */
  new += 2 * VARSIZE;

  return (new-VARSIZE);
}

TAGGED *copy_struct(term, copy_state)
     TAGGED term;
     copy_rec *copy_state;
{
  TAGGED *new;
  TAGGED *old;

  new = copy_state->new_top;
  old = TagToPointer(term);

  new[0] = old[0];		/* copy term       */
  old[0] = CopyMark(&(new[0]));	/* update oldspace */

  if(VARSIZE==2)
    new[1] = old[1];

  for(i=VARSIZE ; i < (size+1)*VARSIZE ; i+=VARSIZE)
    {
      new[i] = RemoveMarkF(old[i]); /* copy term       */
      old[i] = CopyMark(&(new[i])); /* update oldspace */

      if(VARSIZE==2)
	new[i+1] = old[i+1];
    }

  Mark(new[0]);			/* mark first cell */
  new += (size + 1) * VARSIZE;

  return (new-VARSIZE);
}
