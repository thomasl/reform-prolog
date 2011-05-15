/*
 * engine.h
 *
 * Johan Bevemyr.....Thu May 23 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef ENGINE_H
#define ENGINE_H


#if defined(PARALLEL)
enum {
  DYNAMIC,
  STATIC
};

enum {
  LEFT_BODY,
  RIGHT_BODY
};

enum {
  HORIZONTAL,
  VERTICAL
};

#define LEVEL_LOCK        -100000L

#if defined(LOCK_BIND)

#define GrabLevel(Lev,W)					\
{								\
  register volatile s32 *gl_pos = &(W->global->sched_level);	\
								\
  LockAddress(gl_pos);						\
  Lev = *gl_pos;						\
}

#define DropLevel(Lev,W)					\
{								\
  register volatile s32 *gl_pos = &(W->global->sched_level);	\
								\
  *gl_pos = Lev;						\
  ReleaseAddress(gl_pos);					\
}

#else /* not LOCK_BIND */

#define GrabLevel(Lev,W)					\
{								\
  register volatile s32 *gl_pos = &(W->global->sched_level);	\
								\
  KSR1(DECLARE_SWAP_VAL);					\
								\
  while((Lev = swap_il(gl_pos,LEVEL_LOCK)) == LEVEL_LOCK)	\
    {								\
      while(*gl_pos == LEVEL_LOCK);				\
    }								\
}

#define DropLevel(Lev,W)					\
{								\
  register volatile s32 *gl_pos = &(W->global->sched_level);	\
  *gl_pos = Lev;						\
}

#endif /* LOCK_BIND */

#define NO_ENV_ID	((natural) -1)	/* too large environment, max size is 255 */
  
#endif /* PARALLEL */


enum {
  Var_Label = 0,
  ATM_Label,
  NUM_Label,
  LST_Label,
  STR_Label
};

#define SetY(Y)      yreg = Y

#define G(N)        (w->global->global_regs[N])

#define X(N)        (areg[N])
#define Y(N)        (yreg[N*SVASIZE])
#define Xw(N)       (w->regs[N])
#define Yw(N)       (w->frame->yreg[N*SVASIZE])
#define Yf(N)       (frame->yreg[N*SVASIZE])
#define Xc(N)       (choice->areg[ARSIZE*N])

#define Leap_Label_Offset(PC,Off)		\
{						\
  register s32 p1 = *(PC + Off);		\
						\
  if (p1 == 0)					\
    goto fail;					\
  else						\
    PC = ((code *) (((s32) PC) + p1)) + Off;	\
}

#define DispatchLabel(PC,Off)  (((code *)(((s32)PC)+((s32) *(PC+Off))))+Off)

#define Dispatch(PC,Off)			\
{						\
  Leap_Label_Offset(PC,Off);			\
  Execute_Read_Instr;				\
}

#if defined(THREADED_CODE)

typedef void *c_address;

extern c_address *global_label_table;


#if defined(PREFETCH)

#define Execute_Read_Instr				\
{							\
  register c_address prefetch = (c_address)(*pc++);	\
							\
  Pre_DisplayInstr("read",0);				\
  writemode = FALSE;					\
  goto *prefetch;					\
}

#define Execute_Write_Instr				\
{							\
  register c_address prefetch = (c_address)(*pc++);	\
							\
  Pre_DisplayInstr("write",0);        			\
  writemode = TRUE;                   			\
  goto *prefetch;					\
}

#define Pre_FetchInit    register c_address prefetch
#define Pre_Fetch        prefetch = (c_address)(*pc++)
#define Pre_FetchTop(C)  register c_address prefetch = (c_address)(*(pc+C))

#define Pre_Execute_Read_Instr(C)		\
{						\
  Pre_DisplayInstr("read",C);			\
  writemode = FALSE;                  		\
  pc += C;					\
  goto *prefetch;				\
}

#define Pre_Execute_Write_Instr(C)		\
{						\
  Pre_DisplayInstr("write",C);			\
						\
  writemode = TRUE;				\
  pc += C;					\
  goto *prefetch;				\
}

#else /* not PREFETCH */

#define Execute_Read_Instr			\
{						\
  DisplayInstr("read");				\
  writemode = FALSE;				\
  goto *(c_address)(*pc++);			\
}

#define Execute_Write_Instr			\
{						\
  DisplayInstr("write");			\
  writemode = TRUE;				\
  goto *(c_address)(*pc++);			\
}

#define Pre_FetchInit         
#define Pre_Fetch                
#define Pre_FetchTop(C)             

#define Pre_Execute_Read_Instr(C)  Execute_Read_Instr 
#define Pre_Execute_Write_Instr(C) Execute_Write_Instr

#endif /* PREFETCH */


#define WriteModeDispatch(L)    { if (writemode) goto L; }


#define FrameSize(CP)    	((indx)*((code *) (CP-2)))
#define StoreFrameSize(CP,Size) 					   \
{									   \
  code *sizepos;							   \
  									   \
  sizepos = ((code *) (CP));						   \
									   \
  sizepos -= 2;								   \
  									   \
  *sizepos = ((code) Size);						   \
}

#define Code(PC)		*(PC++)
#define UnCode(PC)  		(--PC)
#define Get_Code(PC)		*(PC++)
#define Get_Index(PC) 		(indx)*(PC++)
#define Inc_Index(PC)           (PC++)
#define Get_Instr(X)            (X)
#define Get_Op(X)               Get_Instr(X)

#define Get_Index_I(N,I)        Get_Index(pc)
#define Get_Index_I_M(N,I)      Get_Index(pc)

#define InstToOp(To, X)							      \
{									      \
  register s32 i = 0;							      \
  register s32 TMP_X = (s32)(X);					      \
									      \
  while ((i < END_OF_PRED) && (((c_address)TMP_X) != global_label_table[i]))  \
    i++;								      \
									      \
  To = i;								      \
}

#define Get_Instruction(PC)     1

#else /* not THREADED_CODE */

#define Execute_Read_Instr      goto instructions
#define Execute_Write_Instr     goto write_instructions

#define WriteModeDispatch(L)

#define FrameSize(CP)    	Get_Index_I(2,*((code *) (CP-2)))
#define StoreFrameSize(CP,Size) 					   \
{									   \
  code *sizepos;							   \
  									   \
  sizepos = ((code *) (CP));						   \
									   \
  sizepos -= 2;								   \
  									   \
  *sizepos = ((code) ((Size) << (INDEXOFFSET * 2)));                       \
}

#define Code(PC)		*(PC++)
#define UnCode(PC)  		(--PC)
#define Get_Code(PC)		*(PC++)
#define Get_Index(PC) 		(indx)*(PC++)
#define Inc_Index(PC)           (PC++)
#define Get_Instr(X)            ((int) ((X) & INDEXMASK))
#define Get_Op(X)               Get_Instr(X)

#define Get_Index_I(N,I)        ((int) ((I) >> (INDEXOFFSET * N)))
#define Get_Index_I_M(N,I)      Get_Instr(((int) ((I) >> (INDEXOFFSET * N))))
#define InstToOp(To, X)         To = X
#define Get_Instruction(PC)     Get_Code(PC)

#endif /* THREADED_CODE */


#if ! defined(PREFETCH)

#define Pre_FetchInit         
#define Pre_Fetch                
#define Pre_FetchTop(C)             

#define Pre_Execute_Read_Instr(C)  Execute_Read_Instr 
#define Pre_Execute_Write_Instr(C) Execute_Write_Instr

#endif /* PREFETCH */


#define Get_Label(PC)	        (int)*(PC++)
#define Inc_Label(PC)           PC++
#define Get_Label_No_Inc(PC)    (int)*(PC)
#define Get_Tagged(PC) 		(TAGGED)*(PC++)
#define Get_Functor(PC)  	(TAGGED)*(PC++)
#define Get_Definition(PC)  	(definition*)*(PC++)
#define Get_PC_No_Inc(PC)       (code*)*(PC)
#define Get_UseArgs(PC)         (TAGGED *) (PC)
#define CodeT(PC)               ((code *) (PC))

#define Get_Local_Stack_Top  Get_Stack_Top(w,FrameSize(w->next_instr))

#define EnvTop(Frame,FSize)      ((choicepoint *) (((s32) (Frame)) +        \
                                 (((s32) (FSize))*sizeof(TAGGED)*SVASIZE) + \
                                 sizeof(environment) - sizeof(TAGGED)))

#define CpTop(Cp)                ((choicepoint *) (((s32) (Cp)) +           \
                                 ((Cp)->arity*sizeof(TAGGED)*ARSIZE) +      \
                                 sizeof(choicepoint) - sizeof(TAGGED)))     

#define Get_Stack_Top(W,S)                                                  \
  (((choicepoint *) W->frame) > W->choice ? EnvTop(W->frame,S) :            \
                                            CpTop(W->choice))


/* Pack live x-registers prior to garbage collection.
 */
#define PreGC_PackLiveList(live_list, size)	\
{						\
  register integer i;				\
						\
  for (i = size - 1; i >= 0; i--)		\
    {						\
      X(i) = X(live_list[i]);			\
    }						\
}

/* Unpack live x-registers after garbage collection.
 */
#define PostGC_UnpackLiveList(live_list, size)	\
{						\
  register integer i;				\
						\
  for (i = size - 1; i >= 0; i--)		\
    {						\
      X(live_list[i]) = X(i);			\
    }						\
}


/* Pointers to the meta interpreter.
 */
extern definition *interpret_conj;
extern definition *interpret_wake;

extern definition *interpret_goal;
extern definition *interpret_goal_spy;
extern definition *interpret_goal_trace;

extern code start_pred[];


extern PROTOTYPE(void, engine, (definition *, worker *));


#endif /* ENGINE_H */
