/*
 * array.h
 *
 * Johan Bevemyr.....Tue Apr  7 1992
 * Patric Hedlin.....Thu Jul  6 1995
 *
 *
 * Description:
 *
 *  o	array
 *  o	array_arg
 *  o	array_setarg
 *  o	array_sizeof
 *  o	array_reduce
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef ARRAY_H
#define ARRAY_H


#define sizeofArray(size)	(sizeof(generic) / sizeof(uword) + size * VARSIZE)

#define IsArray(term)		(IsGEN(term) && (GetGenTag(term) == &array_method_table))

#define GetArrayHeader(term)	(Generic(RemoveTag(term, GEN))->header)
#define GetArraySize(term)	(Generic(RemoveTag(term, GEN))->data[0])
#define GetArrayArg(term,index)	(Generic(RemoveTag(term, GEN))->data[index * VARSIZE])


extern method array_method_table;


#endif /* ARRAY_H */
