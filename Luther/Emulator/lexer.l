


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
