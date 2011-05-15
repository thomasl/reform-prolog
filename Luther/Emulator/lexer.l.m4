/*
 * lexer.l.m4 - A lex grammar for WAM-code in Luther.
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

#include <stdio.h>

#include "parser.h"
#include "luther.h"

#if defined(sequent) /* bug in libraries on echnaton */
#undef abs(_)
#endif 

#if defined(THINK_C)
YY_DECL;
#endif

int current_line;

#if defined(YY_CURRENT_BUFFER)

#define MAX_LOAD_REC_DEPTH  20

typedef struct loadbuf {
  YY_BUFFER_STATE yybuf;
  int line_nr;
} loadbuf_t;

static loadbuf_t include_stack[MAX_LOAD_REC_DEPTH];
static int load_rec_ptr = 0;
static int current_buffer;

void luther_switch_to_buffer(buf)
  int buf;
{
  yy_switch_to_buffer(include_stack[buf].yybuf);
  current_line = include_stack[buf].line_nr;
  current_buffer = buf;
}

void luther_save_linecount(buf)
   int buf;
{
  include_stack[buf].line_nr = current_line;
}

long luther_save_current_buffer()
{
  register long i;

  for (i = 0; include_stack[i].yybuf != NULL && i < MAX_LOAD_REC_DEPTH; i++);

  if (i == MAX_LOAD_REC_DEPTH)
    {
      FatalError("To many files being loaded");	
    }
 
  include_stack[i].yybuf = YY_CURRENT_BUFFER;
  include_stack[i].line_nr = 0;

  current_buffer = i;
  current_line = 1;

  return i;
}

void luther_init_buffers()
{
  register int i;

  for (i = 0; i < MAX_LOAD_REC_DEPTH; i++)
    include_stack[i].yybuf = NULL;
}

void luther_new_buffer(file)
   FILE *file;
{
  yy_switch_to_buffer(yy_create_buffer(file,YY_BUF_SIZE));
}

void luther_delete_buffer(buf)
   s32 buf;
{
  yy_delete_buffer(include_stack[buf].yybuf);
  include_stack[buf].yybuf = NULL;
}

#else /* not YY_CURRENT_BUFFER */

void luther_save_linecount(_buf) { }

void luther_switch_to_buffer(_buf) { }

s32 luther_save_current_buffer()
{
  return current_line = 1;
}

void luther_init_buffers() { }

void luther_new_buffer(_file) { }

void luther_delete_buffer(_buf) { }

#endif /* YY_CURRENT_BUFFER */
%}

%%
"Switch_On_Term"	{ return Switch_On_Term; }
"Switch_On_Constant"	{ return Switch_On_Constant; }
"Switch_On_Structure"	{ return Switch_On_Structure; }
	
"Try"			{ return Try; }
"Retry"			{ return Retry; }
"Trust"			{ return Trust; }
"Try_Me_Else"		{ return Try_Me_Else; }
"Retry_Me_Else"		{ return Retry_Me_Else; }
"Trust_Me"		{ return Trust_Me; }
	
"Choice_X"		{ return Choice_X; }
"Choice_Y"		{ return Choice_Y; }
"Cut"			{ return Cut; }
"Cut_X"			{ return Cut_X; }
"Cut_Y"			{ return Cut_Y; }
"Inline"		{ return Inline; }
"Builtin"		{ return Builtin; }
"Meta_Call"		{ return Meta_Call; }
"Meta_Execute"		{ return Meta_Execute; }
	
"Require"		{ return Require; }
"Require_Using"		{ return Require_Using; }
"Allocate"		{ return Allocate; }
"Allocate2"		{ return Allocate2; }
"Deallocate"		{ return Deallocate; }
"Init"			{ return Init; }
"Call"			{ return Call; }
"Execute"		{ return Execute; }
"Proceed"		{ return Proceed; }
"Fail"			{ return Fail; }
	
"Get_X_Variable"	{ return Get_X_Variable; }
"Get_Y_Variable"	{ return Get_Y_Variable; }
"Get_Y_First_Value"	{ return Get_Y_First_Value; }
"Get_X_Value"		{ return Get_X_Value; }
"Get_Y_Value"		{ return Get_Y_Value; }
"Get_Constant"		{ return Get_Constant; }
"Get_Nil"		{ return Get_Nil; }
"Get_Structure"		{ return Get_Structure; }
"Get_List"		{ return Get_List; }
"Get_Constant_X0"	{ return Get_Constant_X0; }
"Get_Nil_X0"		{ return Get_Nil_X0; }
"Get_Structure_X0"	{ return Get_Structure_X0; }
"Get_List_X0"		{ return Get_List_X0; }

"Put_X_Void"		{ return Put_X_Void; }
"Put_Y_Void"		{ return Put_Y_Void; }
"Put_X_Variable"	{ return Put_X_Variable; }
"Put_Y_Variable"	{ return Put_Y_Variable; }
"Put_X_Value"		{ return Put_X_Value; }
"Put_Y_Value"		{ return Put_Y_Value; }
"Put_X_Unsafe_Value"	{ return Put_X_Unsafe_Value; }
"Put_Y_Unsafe_Value"	{ return Put_Y_Unsafe_Value; }
"Put_Constant"		{ return Put_Constant; }
"Put_Nil"		{ return Put_Nil; }
"Put_Structure"		{ return Put_Structure; }
"Put_List"		{ return Put_List; }
	
"Unify_Void"		{ return Unify_Void; }
"Unify_X_Variable"	{ return Unify_X_Variable; }
"Unify_Xvar_Xvar"	{ return Unify_Xvar_Xvar; }
"Unify_Y_Variable"	{ return Unify_Y_Variable; }
"Unify_Y_First_Value"	{ return Unify_Y_First_Value; }
"Unify_X_Value"		{ return Unify_X_Value; }
"Unify_Y_Value"		{ return Unify_Y_Value; }
"Unify_X_Local_Value"	{ return Unify_X_Local_Value; }
"Unify_Y_Local_Value"	{ return Unify_Y_Local_Value; }
"Unify_Constant"	{ return Unify_Constant; }
"Unify_Nil"		{ return Unify_Nil; }
"Unify_Structure"	{ return Unify_Structure; }
"Unify_List"		{ return Unify_List; }


"X1"	{ return Switch_On_Term; }
"X2"	{ return Switch_On_Constant; }
"X3"	{ return Switch_On_Structure; }
	
"X4"	{ return Try; }
"X5"	{ return Retry; }
"X6"	{ return Trust; }
"X7"	{ return Try_Me_Else; }
"X8"	{ return Retry_Me_Else; }
"X9"	{ return Trust_Me; }
	
"X10"	{ return Choice_X; }
"X11"	{ return Choice_Y; }
"X12"	{ return Cut; }
"X13"	{ return Cut_X; }
"X14"	{ return Cut_Y; }
"X15"	{ return Inline; }
"X16"	{ return Builtin; }
"X17"	{ return Meta_Call; }
"X18"	{ return Meta_Execute; }
	
"X19"	{ return Require; }
"X20"	{ return Require_Using; }
"X21"	{ return Allocate; }
"X22"	{ return Allocate2; }
"X23"	{ return Deallocate; }
"X24"	{ return Init; }
"X25"	{ return Call; }
"X26"	{ return Execute; }
"X27"	{ return Proceed; }
"X28"	{ return Fail; }
	
"X29"	{ return Get_X_Variable; }
"X30"	{ return Get_Y_Variable; }
"X31"	{ return Get_Y_First_Value; }
"X32"	{ return Get_X_Value; }
"X33"	{ return Get_Y_Value; }
"X34"	{ return Get_Constant; }
"X35"	{ return Get_Nil; }
"X36"	{ return Get_Structure; }
"X37"	{ return Get_List; }
"X38"	{ return Get_Constant_X0; }
"X39"	{ return Get_Nil_X0; }
"X40"	{ return Get_Structure_X0; }
"X41"	{ return Get_List_X0; }

"X42"	{ return Put_X_Void; }
"X43"	{ return Put_Y_Void; }
"X44"	{ return Put_X_Variable; }
"X45"	{ return Put_Y_Variable; }
"X46"	{ return Put_X_Value; }
"X47"	{ return Put_Y_Value; }
"X48"	{ return Put_X_Unsafe_Value; }
"X49"	{ return Put_Y_Unsafe_Value; }
"X50"	{ return Put_Constant; }
"X51"	{ return Put_Nil; }
"X52"	{ return Put_Structure; }
"X53"	{ return Put_List; }
	
"X54"	{ return Unify_Void; }
"X55"	{ return Unify_X_Variable; }
"X56"	{ return Unify_Y_Variable; }
"X57"	{ return Unify_Y_First_Value; }
"X58"	{ return Unify_X_Value; }
"X59"	{ return Unify_Y_Value; }
"X60"	{ return Unify_X_Local_Value; }
"X61"	{ return Unify_Y_Local_Value; }
"X62"	{ return Unify_Constant; }
"X63"	{ return Unify_Nil; }
"X64"	{ return Unify_Structure; }
"X65"	{ return Unify_List; }


changequote([,])
ifdef([BOUNDED_Q],[
define([JUMP_CODE],1)
"zerop"                         { return Zerop; }
"listp"                         { return Listp; }
"deterministic"                 { return Deterministic; }

"allocate*"                     { return Allocate_Star; }
"repeat"                        { return Repeat; }
"iterate_int"                   { return Iterate_Int; }
"iterate_int*"                  { return Iterate_Int_Star; }
"iterate_list"                  { return Iterate_List; }
"iterate_list*"                 { return Iterate_List_Star; }
"ensure_list_try"		{ return Ensure_List_Try; }
"ensure_list_trust"             { return Ensure_List_Trust; }
])

ifdef([PARALLEL_BQ],[
"spawn_left_bq"                 { return Spawn_Left_BQ; }
])

ifdef([PARALLEL],[
define([JUMP_CODE],1)

"Global_Get_X_Value"		{ return Global_Get_X_Value; }
"Global_Get_Y_Value"		{ return Global_Get_Y_Value; }
"Global_Get_Constant"		{ return Global_Get_Constant; }
"Global_Get_Nil"		{ return Global_Get_Nil; }
"Global_Get_Structure"		{ return Global_Get_Structure; }
"Global_Get_List"		{ return Global_Get_List; }
"Global_Get_Constant_X0"	{ return Global_Get_Constant_X0; }
"Global_Get_Nil_X0"		{ return Global_Get_Nil_X0; }
"Global_Get_Structure_X0"	{ return Global_Get_Structure_X0; }
"Global_Get_List_X0"		{ return Global_Get_List_X0; }

"Global_Unify_X_Value"		{ return Global_Unify_X_Value; }
"Global_Unify_Y_Value"		{ return Global_Unify_Y_Value; }
"Global_Unify_X_Local_Value"	{ return Global_Unify_X_Local_Value; }
"Global_Unify_Y_Local_Value"	{ return Global_Unify_Y_Local_Value; }
"Global_Unify_Constant"		{ return Global_Unify_Constant; }
"Global_Unify_Nil"		{ return Global_Unify_Nil; }
"Global_Unify_Structure"	{ return Global_Unify_Structure; }
"Global_Unify_List"		{ return Global_Unify_List; }

"Build_Rec_Poslist"		{ return Build_Rec_Poslist; }
"Build_Poslist"			{ return Build_Poslist; }
"Build_Poslist_Value"		{ return Build_Poslist_Value; }
"Build_Neglist"			{ return Build_Neglist; }
"Build_Neglist_Value"		{ return Build_Neglist_Value; }
"Build_Variables"		{ return Build_Variables; }

"Put_Nth_Head"			{ return Put_Nth_Head; }
"Put_Nth_Tail"			{ return Put_Nth_Tail; }
"Put_Global_Arg"		{ return Put_Global_Arg; }
"Nil_Nth_Head"			{ return Nil_Nth_Head; }

"Unify_Nth_Head"		{ return Unify_Nth_Head; }
"Unify_Nth_Tail"		{ return Unify_Nth_Tail; }
"Unify_Global_Arg"		{ return Unify_Global_Arg; }

"Start_Right_Body"		{ return Start_Right_Body; }
"Start_Left_Body"		{ return Start_Left_Body; }

"Initialize_Right"		{ return Initialize_Right; }
"Initialize_Left"		{ return Initialize_Left; }

"Spawn_Right"			{ return Spawn_Right; }
"Spawn_Left"			{ return Spawn_Left; }

"Await_Leftmost"		{ return Await_Leftmost; }
"Await_Nonvar"			{ return Await_Nonvar; }
"Await_Strictly_Nonvar"		{ return Await_Strictly_Nonvar; }
"Await_Variable"		{ return Await_Variable; }

"Par_Builtin"			{ return Par_Builtin; }
"Par_Inline"			{ return Par_Inline; }

"Lock_And_Get_Structure"	{ return Lock_And_Get_Structure; }
"Lock_And_Get_List"		{ return Lock_And_Get_List; }
"Unlock"			{ return Unlock; }


"X66"	{ return Global_Get_X_Value; }
"X67"	{ return Global_Get_Y_Value; }
"X68"	{ return Global_Get_Constant; }
"X69"	{ return Global_Get_Nil; }
"X70"	{ return Global_Get_Structure; }
"X71"	{ return Global_Get_List; }
"X72"	{ return Global_Get_Constant_X0; }
"X73"	{ return Global_Get_Nil_X0; }
"X74"	{ return Global_Get_Structure_X0; }
"X75"	{ return Global_Get_List_X0; }
"X76"	{ return Global_Unify_X_Value; }
"X77"	{ return Global_Unify_Y_Value; }
"X78"	{ return Global_Unify_X_Local_Value; }
"X79"	{ return Global_Unify_Y_Local_Value; }
"X80"	{ return Global_Unify_Constant; }
"X81"	{ return Global_Unify_Nil; }
"X82"	{ return Global_Unify_Structure; }
"X83"	{ return Global_Unify_List; }

"X84"	{ return Build_Rec_Poslist; }
"X85"	{ return Build_Poslist; }
"X86"	{ return Build_Poslist_Value; }
"X87"	{ return Build_Neglist; }
"X88"	{ return Build_Neglist_Value; }
"X89"	{ return Build_Variables; }

"X90"	{ return Put_Nth_Head; }
"X91"	{ return Put_Nth_Tail; }
"X92"	{ return Put_Global_Arg; }
"X93"	{ return Nil_Nth_Head; }

"X94"	{ return Unify_Nth_Head; }
"X95"	{ return Unify_Nth_Tail; }
"X96"	{ return Unify_Global_Arg; }

"X97"	{ return Start_Right_Body; }
"X98"	{ return Start_Left_Body; }

"X99"	{ return Initialize_Right; }
"X100"	{ return Initialize_Left; }

"X101"	{ return Spawn_Right; }
"X102"	{ return Spawn_Left; }

"X103"	{ return Await_Leftmost; }
"X104"	{ return Await_Nonvar; }
"X105"	{ return Await_Strictly_Nonvar; }
"X106"	{ return Await_Variable; }

"X107"	{ return Par_Builtin; }
"X108"	{ return Par_Inline; }

"X109"	{ return Lock_And_Get_Structure; }
"X110"	{ return Lock_And_Get_List; }
"X111"	{ return Unlock; }
])

ifdef([JUMP_CODE],[
"Jump"  { return Jump; }
"X112"	{ return Jump; }
])
changequote(`,')


"Label"				{ return Label; }

"Predicate"			{ return PREDICATE; }
"Directive"			{ return DIRECTIVE; }

"-"?[0-9]*"."[0-9]+		{ return FLOAT; }
"-"?[0-9]*"."[0-9]+[eE]"-"?[0-9]+ { return FLOAT; }
"-"?[0-9]+			{ return NUMBER; }
"-"?[0-9]+[eE]"-"[0-9]+	        { return FLOAT; }
"-"?[0-9]+[eE][0-9]+	        { return NUMBER; }

"[]"				{ return ATOMNAME; }
"{}"				{ return ATOMNAME; }

[!#$&*+\-\./0-9:;<=>?@\\^_a-z~][!#$&*+\-\./0-9:;<=>?@A-Z\\^_a-z~]* { return ATOMNAME; }

"'"([\01-\46\50-\377]|"''")*"'" {
  /* move the characters one step to the left to avoid the quote */
                                  int i = 0;
                                  int j = 1;

                                  parseatom:
                                    if (yytext[j] == 39)
                                      /* j is pointing to a quote */
                                      if (yytext[j+1] == 39)
					{
					  j++; /* it was a quoted quote, continue */
					}
                                      else /* place a end-of-string at the end */
					{
					  yytext[i] = 0;
					  return ATOMNAME;
					}
                                   yytext[i] = yytext[j];
                                   i++; j++;
                                   goto parseatom;
                                }

"("				{ return LPAR; }
")"				{ return RPAR; }
"{"                             { return LSPAR; }
"}"                             { return RSPAR; }
	
[\n]				{ current_line++; }
[\t ]                           ;
%.*$				;
<<EOF>>				{ return END; }
%%

#if ! defined(yywrap)
/* Needed for lex, though not flex. */
int yywrap() { return 1; }
#endif
