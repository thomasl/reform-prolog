


/*
 * parser.y.m4 - A YACC grammar for WAM-code in Luther.
 *
 * Johan Bevemyr.....Fri May 24 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

%{

/*   The linear search problem with switch_on_constant and switch_on_structure
 * has a solution. The problem is that if we simply sort the array of constant-
 * label records then the labels will be scrambled (they are relative to their
 * location). The solution is to use a sorting algorithm that adjusts the label
 * when it moves a record.
 */

#include "luther.h"
#include "labelsort.h"
#include "inline.h"
#include "bignum.h"
#include "engine.h"
#include "load.h"

#if defined(THINK_C)
static int yyerror PROTO((char *));
#endif

#if defined(THREADED_CODE)
#  define Init_Code(C)		Set_Code(C)
#  define Flush_Instruction  
#  define Set_S_Tagged       

#  define Set_Code(C)		Set_Tagged(global_label_table[(int)C])

#  define Set_Index(I)		Set_Tagged(I)

#  define Set_Small_Index(I)	Set_Index(I)
#  define Save_Tagged(T)	Set_Tagged(T)
#else
#  define Init_Code(C)		{ instruction = C; index_nr = 0; }
#  define Flush_Instruction	Set_Code(instruction)
#  define Set_S_Tagged		Set_Tagged(savedtagged)

#  define Set_Code(C)		Set_Tagged(C)

#  define Set_Index(I)		Set_Tagged(I)

#  define Set_Small_Index(I)			\
{						\
  index_nr += 1;				\
  if (index_nr > MAX_INDEX)			\
    {						\
      Flush_Instruction;			\
      instruction = 0xffL;			\
      index_nr = 1;				\
    }						\
    instruction |= ((code)(I << index_nr*8));	\
}

#  define Save_Tagged(T)	savedtagged = T

#endif /* THREADED_CODE */


#define Set_Functor(F)     Set_Tagged(F)

#define Set_Definition(D)  Set_Tagged(D)

#define Set_Label(L)       Set_Tagged(L)

#define Set_Tagged(T)					\
{							\
  *(parse_worker->global->code_current) = (code)(T);	\
    parse_worker->global->code_current++;		\
}


#define Reset_Switch_Stack   sw_stack = sw_stack_start;

#define Push_Switch_Table(C)			\
{						\
  *sw_stack = C;				\
   sw_stack++;					\
}

#define Sort_Switch_Tables				\
{							\
  while (sw_stack != sw_stack_start)			\
    {							\
      sw_stack--;					\
      labelsort(*sw_stack,((int)*(*sw_stack - 1)));	\
    }							\
}

#define YYDEBUG 1
#define MAX_NR_SWITCHES_IN_CLAUSE 10

/* Used to set labelreferences to their proper adresses */    

typedef struct backpatch {
  s32 label;
  code *address;
  struct backlist *prev;
} backpatch;

typedef struct backlist {
  code *address;
  struct backlist *next;
} backlist;

static backpatch backarray[MAXLABELNUMBER];
static s32 backcount;

static TAGGED predname;
static s32 predarity;

static TAGGED functorname;
static s32 functorarity;

static TAGGED instruction;
static TAGGED savedtagged;

static code **sw_stack, *sw_stack_start[MAX_NR_SWITCHES_IN_CLAUSE];


/* local prototypes */

static long get_label_address PROTO((s32, worker *));
static backlist *make_back PROTO((code *, backlist *, worker *));
static void bpatch PROTO((s32, code *));


/* Defined by bison/yacc: */

int yyparse PROTO((void));


/* Defined by (f)lex: */

#if defined(YYTEXT_POINTER)
extern char *yytext;
#else
extern char  yytext[];
#endif

extern FILE *yyin;
extern int yylex PROTO((void));
extern int current_line;

/* Defined by storage.c */

extern heap *patchheapstart;
extern heap *patchheapcurrent;

worker *parse_worker;

/* used for index packing */

static int index_nr;

%}	

%token Switch_On_Term Switch_On_Constant Switch_On_Structure 
%token Try Retry Trust Try_Me_Else Retry_Me_Else Trust_Me 
%token Choice_X Choice_Y Cut Cut_X Cut_Y Inline Builtin Meta_Call 
%token Meta_Execute

%token Require Require_Using
%token Allocate Allocate2 Deallocate Init Call Execute Proceed Fail 
%token Get_X_Variable Get_Y_Variable Get_Y_First_Value Get_X_Value 
%token Get_Y_Value Get_Constant Get_Nil Get_Structure Get_List Get_Constant_X0 
%token Get_Nil_X0 Get_Structure_X0 Get_List_X0 

%token Put_X_Void Put_Y_Void Put_X_Variable Put_Y_Variable Put_X_Value 
%token Put_Y_Value Put_X_Unsafe_Value Put_Y_Unsafe_Value Put_Constant 
%token Put_Nil Put_Structure Put_List 
%token Unify_Void Unify_X_Variable Unify_Xvar_Xvar Unify_Y_Variable
%token Unify_Y_First_Value 
%token Unify_X_Value Unify_Y_Value Unify_X_Local_Value Unify_Y_Local_Value 
%token Unify_Constant Unify_Nil Unify_Structure Unify_List









%token Halt Noop Label
%token PREDICATE DIRECTIVE ATOMNAME LPAR RPAR LSPAR RSPAR NUMBER FLOAT END


%%
  
start 	: preds end

end	: END
          { 
	    parser_return_value = PARSE_EOF;
            YYACCEPT;
	  }
	;

preds 	: preds pred
	|
	;

pred	: PREDICATE LPAR ATOMNAME
	  { predname = store_atom(yytext,parse_worker);
	  } NUMBER 
	  { predarity = atoi(yytext); 

	    if (parse_worker->global->code_current > 
		parse_worker->global->code_limit)
	       {
	          extend_codespace(parse_worker);
               }

	    backcount = 0;
	    reset_backpatchheap(parse_worker);
	    Reset_Switch_Stack;
	    store_emulated_predicate(StoreFunctorW(predname,predarity,
						   parse_worker),
				     parse_worker->global->code_current,
				     parse_worker);
	  }  codes RPAR 
	  { 
	    Sort_Switch_Tables;
	    if(parse_worker->global->flags.load_verbose)
		printf("{ %s/%d defined }\n", GetString(predname,parse_worker),
		       predarity);
	    Set_Code(END_OF_PRED);
	    if(parse_worker->global->code_current > 
               parse_worker->global->code_end) {
		FatalError("codespace exhausted");
	    }
	  }
	| DIRECTIVE LPAR ATOMNAME
	  { predname = store_atom(yytext,parse_worker);
	  } NUMBER 
	  { predarity = atoi(yytext); 
	    backcount = 0;
	    reset_backpatchheap(parse_worker);
	    Reset_Switch_Stack;
	    store_emulated_predicate(StoreFunctorW(predname,predarity,
						  parse_worker),
				     parse_worker->global->code_current,
				     parse_worker);
	  }  codes RPAR 
	  { 
	    Sort_Switch_Tables;
	    if(parse_worker->global->flags.load_verbose)
		printf("{ %s/%d defined }\n", GetString(predname,parse_worker),
		       predarity);
	    Set_Code(END_OF_PRED);
	    if(parse_worker->global->code_current > 
               parse_worker->global->code_end) {
		FatalError("codespace exhausted");
	    }
	    parser_return_value = PARSE_DIRECTIVE;
	    YYACCEPT;
	  }
	;

codes	: codes code
	|
	;

code	: Switch_On_Term { Set_Code(SWITCH_ON_TERM); } 
	      label label label label label 
        | Switch_On_Constant { Set_Code(SWITCH_ON_CONSTANT); } big_index
	      { Push_Switch_Table(parse_worker->global->code_current); }
	      LPAR consts RPAR label 
        | Switch_On_Structure { Set_Code(SWITCH_ON_STRUCTURE); } big_index 
	      { Push_Switch_Table(parse_worker->global->code_current); }
	      LPAR structs RPAR label 
        | Try { Init_Code(TRY); 
                Set_Small_Index(predarity); Flush_Instruction; } label 
        | Retry { Set_Code(RETRY); } label 
        | Trust { Set_Code(TRUST); } label 
        | Try_Me_Else { Init_Code(TRY_ME_ELSE); 
                        Set_Small_Index(predarity); Flush_Instruction; } label 
	| Retry_Me_Else { Set_Code(RETRY_ME_ELSE); } label 
	| Trust_Me { Set_Code(TRUST_ME); } 
	| Choice_X { Init_Code(CHOICE_X); } small_index { Flush_Instruction; } 
	| Choice_Y { Init_Code(CHOICE_Y); } small_index { Flush_Instruction; } 
	| Cut { Set_Code(CUT); } 
	| Cut_X { Init_Code(CUT_X); } small_index { Flush_Instruction; } 
	| Cut_Y { Init_Code(CUT_Y); } small_index { Flush_Instruction; } 
	| Inline { Init_Code(INLINE); } builtin { Flush_Instruction; } 
          label big_indexes 
	| Builtin { Init_Code(BUILTIN); } builtin { Flush_Instruction; } 
          big_indexes 
	| Meta_Call { Init_Code(META_CALL); } small_index small_index 
	  	{ 
#if ! defined(THREADED_CODE)
                  Flush_Instruction; savedtagged = 0xffL; Set_S_Tagged; 
#else
	          TAGGED tmp = *(parse_worker->global->code_current-1);
		  *(parse_worker->global->code_current-1) = 
			*(parse_worker->global->code_current-2);
		  *(parse_worker->global->code_current-2) = tmp;
#endif
	        } 
	| Meta_Execute { Init_Code(META_EXECUTE); } small_index 
		{ Flush_Instruction;} 
	  	

	| Require { Set_Code(REQUIRE); } big_index 
	| Require_Using { Init_Code(REQUIRE_USING); } save_index small_index 
		{ Flush_Instruction; Set_S_Tagged; } 
	| Allocate { Init_Code(ALLOCATE); Set_Small_Index(predarity); Flush_Instruction; } 
	| Allocate2 { Init_Code(ALLOCATE2); Set_Small_Index(predarity); Flush_Instruction; } 
	| Deallocate { Set_Code(DEALLOCATE); }  
        | Init { Init_Code(INIT); } small_index { Flush_Instruction;} 
		big_indexes 
	| Call { Init_Code(CALL); } def1 skip_index small_index 
		{ Flush_Instruction; Set_S_Tagged; 
#if defined(THREADED_CODE)
		{
	          TAGGED tmp = *(parse_worker->global->code_current-1);
		  *(parse_worker->global->code_current-1) = 
			*(parse_worker->global->code_current-2);
		  *(parse_worker->global->code_current-2) = tmp;
		}
#endif
		} 
	| Execute { Set_Code(EXECUTE); } def 
	| Proceed { Set_Code(PROCEED); } 
	| Fail { Set_Code(FAIL); } 
	| Get_X_Variable { Init_Code(GET_X_VARIABLE); } small_index 
		small_index { Flush_Instruction; } 
	| Get_Y_Variable { Init_Code(GET_Y_VARIABLE); } small_index 
		small_index { Flush_Instruction; } 
	| Get_Y_First_Value { Init_Code(GET_Y_FIRST_VALUE); } small_index 
		small_index { Flush_Instruction; } 
	| Get_X_Value { Init_Code(GET_X_VALUE); } small_index small_index
                { Flush_Instruction; } 
	| Get_Y_Value { Init_Code(GET_Y_VALUE); } small_index small_index
                { Flush_Instruction; } 
	| Get_Constant { Init_Code(GET_CONSTANT); } const1 small_index 
                { Flush_Instruction; Set_S_Tagged; } 
	| Get_Nil { Init_Code(GET_NIL); } small_index { Flush_Instruction; } 
	| Get_Structure { Init_Code(GET_STRUCTURE); } strct1 small_index 
                { Flush_Instruction; Set_S_Tagged; } 
	| Get_List { Init_Code(GET_LIST); } small_index { Flush_Instruction; } 
	| Get_Constant_X0 { Set_Code(GET_CONSTANT_X0); } const 
	| Get_Nil_X0 { Set_Code(GET_NIL_X0); } 
	| Get_Structure_X0 { Set_Code(GET_STRUCTURE_X0); } strct 
	| Get_List_X0 { Set_Code(GET_LIST_X0); } 



	| Put_X_Void { Init_Code(PUT_X_VOID); } small_index 
		{Flush_Instruction;} 
	| Put_Y_Void { Init_Code(PUT_Y_VOID); } small_index 
		{Flush_Instruction;} 
	| Put_X_Variable { Init_Code(PUT_X_VARIABLE); } small_index small_index
                { Flush_Instruction; } 
	| Put_Y_Variable { Init_Code(PUT_Y_VARIABLE); } small_index small_index
                { Flush_Instruction; } 
	| Put_X_Value { Init_Code(PUT_X_VALUE); } small_index small_index
                { Flush_Instruction; } 
	| Put_Y_Value { Init_Code(PUT_Y_VALUE); } small_index small_index
                { Flush_Instruction; } 
	| Put_X_Unsafe_Value { Init_Code(PUT_X_UNSAFE_VALUE); } small_index 
		small_index { Flush_Instruction; } 
	| Put_Y_Unsafe_Value { Init_Code(PUT_Y_UNSAFE_VALUE); } small_index 
		small_index { Flush_Instruction; } 
	| Put_Constant { Init_Code(PUT_CONSTANT); } const1 small_index 
                { Flush_Instruction; Set_S_Tagged; } 
	| Put_Nil{ Init_Code(PUT_NIL); } small_index { Flush_Instruction; } 
	| Put_Structure { Init_Code(PUT_STRUCTURE); } strct1 small_index 
                { Flush_Instruction; Set_S_Tagged; } 
	| Put_List { Init_Code(PUT_LIST); } small_index { Flush_Instruction; } 
	| Unify_Void { Init_Code(UNIFY_VOID); } small_index 
		{Flush_Instruction;} 
	| Unify_X_Variable { Init_Code(UNIFY_X_VARIABLE); } small_index 
          	{ Flush_Instruction; } 
	| Unify_Xvar_Xvar { Init_Code(UNIFY_XVAR_XVAR); } small_index
		small_index { Flush_Instruction; } 
	| Unify_Y_Variable { Init_Code(UNIFY_Y_VARIABLE); } small_index 
          	{ Flush_Instruction; } 
	| Unify_Y_First_Value { Init_Code(UNIFY_Y_FIRST_VALUE); } small_index
          	{ Flush_Instruction; } 
	| Unify_X_Value { Init_Code(UNIFY_X_VALUE); } small_index 
          	{ Flush_Instruction; } 
	| Unify_Y_Value { Init_Code(UNIFY_Y_VALUE); } small_index 
          	{ Flush_Instruction; } 
	| Unify_X_Local_Value { Init_Code(UNIFY_X_LOCAL_VALUE); } small_index
          	{ Flush_Instruction; } 
	| Unify_Y_Local_Value { Init_Code(UNIFY_Y_LOCAL_VALUE); } small_index 
          	{ Flush_Instruction; } 
	| Unify_Constant { Set_Code(UNIFY_CONSTANT); } const 
	| Unify_Nil { Set_Code(UNIFY_NIL); } 
	| Unify_Structure { Set_Code(UNIFY_STRUCTURE); } strct 
	| Unify_List { Set_Code(UNIFY_LIST); } 













	| Halt { Set_Code(HALT); } 
	| Noop { Set_Code(NOOP); } 
	| Label NUMBER { bpatch(atoi(yytext), 
                                parse_worker->global->code_current); } 
        | error { fprintf(stderr,"[line %d: %s ignored]\n",current_line,
                          yytext); 
	        }
	;

label	: NUMBER { Set_Label(get_label_address(atoi(yytext),parse_worker)); }
	| ATOMNAME { assert(strcmp(yytext,"fail") == STRCMP_EQUAL); Set_Label(NULL); }
	;



big_index  	: NUMBER { Set_Index(atoi(yytext)); }
        	;

big_indexes 	: big_indexes big_index
	        |
	        ;

save_index  	: NUMBER { Save_Tagged(atoi(yytext)); }
        	;

small_index 	: NUMBER { Set_Small_Index(atoi(yytext)); }
		;

skip_index 	: { 
#if ! defined(THREADED_CODE)
			Set_Small_Index(0); 
#endif
		  }
		;
	
builtin : small_index
	| ATOMNAME 
          { int i; 
	    for(i = 0 ; i < INLINE_TABLE_SIZE ; i++) {
	        if(strcmp(inline_table[i].pname,yytext) == STRCMP_EQUAL)
		    break;
            }
	    if(i == INLINE_TABLE_SIZE) {
	        printf("Error - no such builtin function %s\n",yytext);
	        i = 0;
	    }
	    Set_Small_Index(i);
	  }
        ;


consts  : consts const label 
        |
        ;

structs : structs strct label 
        |
        ;

func	: ATOMNAME
	  { functorname = store_atom(yytext,parse_worker); }
	  NUMBER
	  { functorarity = atoi(yytext); }
	;

strct   : func { Set_Functor(StoreFunctorW(functorname,functorarity,
				           parse_worker)); }
	;

strct1  : func { Save_Tagged(StoreFunctorW(functorname,functorarity,
	                                   parse_worker)); }
	;

const	: NUMBER { Set_Tagged(Make_Atomspace_Integer(parse_worker, yytext)); }
        | FLOAT {Set_Tagged(make_atomspace_float(atof(yytext),parse_worker)); }
	| ATOMNAME { Set_Tagged(store_atom(yytext,parse_worker)) ; }
	;
const1	: NUMBER { Save_Tagged(Make_Atomspace_Integer(parse_worker, yytext)); }
        | FLOAT {Save_Tagged(make_atomspace_float(atof(yytext),parse_worker)); }
	| ATOMNAME {Save_Tagged(store_atom(yytext,parse_worker)); }
	;

def	: func { Set_Definition(get_ex_definition(StoreFunctorW(functorname,
							        functorarity,
	                                                        parse_worker),
					          parse_worker)); }
def1	: func { 
		 Save_Tagged((TAGGED) 
				get_c_definition(StoreFunctorW(functorname,
					                       functorarity,
							       parse_worker),
	                                         parse_worker,
						 &instruction));
	       }
	;



%%
int yyerror(s)
     char *s;
{
  fprintf(stderr,"line %d:%s\n",current_line,s);
  return 1;	
}

static backlist *make_back(pc,prev,w)
    code *pc;
    backlist *prev;
    worker *w;
{
  backlist *res;

  res = (backlist *) patch_alloc(sizeof(backlist),w);
  res->address = pc;
  res->next = prev;

  return res;
}

static s32 get_label_address(label,w)
    s32 label;
    worker *w;
{
  s32 i;
  for (i = 0; i != backcount; i++)
    if (backarray[i].label == label)
      if (backarray[i].address != NULL)
	return (s32) (((u32) backarray[i].address)-
		      ((u32) w->global->code_current));
      else
	{
	  backarray[i].prev = make_back(w->global->code_current,
					backarray[i].prev,w);
	  return 0;
	}

  backarray[backcount].label = label;
  backarray[backcount].address = NULL;
  backarray[backcount].prev = make_back(w->global->code_current,NULL,w);
  backcount++;

  return 0;
}


static void bpatch(label,address)
    s32 label;
    code *address;
{
  s32 i;
  backlist *b;
  code *p;

  for (i = 0; i != backcount; i++)
    if (backarray[i].label == label)
      if (backarray[i].address != NULL)
	{
	  Error("Multiply defined labels, using the first");
	  return;
	} 
      else
	{
	  backarray[i].address = address;
	  if (backarray[i].prev != NULL)
	    for (b = backarray[i].prev; b != NULL; b = b->next)
	      {
		p = b->address;
		*p = ((u32) address)-((u32) (b->address));
	      }
	  return;
	}

  backarray[backcount].label = label;
  backarray[backcount].address = address;
  backarray[backcount].prev = NULL;
  backcount++;
}

