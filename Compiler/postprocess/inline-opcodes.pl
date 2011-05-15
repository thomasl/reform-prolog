%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  inline-opcodes.pl
%
%

inline_opcode(Name, Arity, Op, Type, Ret) :-
	 inline_op_code(Name, Arity, Op, Type, Ret), !.

inline_opcode(Name, Arity,_Op,_Type,_Ret) :- 
	error('inknown inline instruction (~w)', [Name/Arity]).


inline_op_code('$compare',		3,  0, predicate, 0).
inline_op_code('$functor',		3,  1, predicate, 0).
inline_op_code('$arg',			3,  2, predicate, 0).
inline_op_code('$univ',			2,  3, predicate, 0).
inline_op_code('$unify',		2,  4, predicate, 0).

inline_op_code('$eq_univ',		2,  5, predicate, 0).
inline_op_code('$ineq_univ',		2,  6, predicate, 0).
inline_op_code('$lt_univ',		2,  7, predicate, 0).
inline_op_code('$gt_univ',		2,  8, predicate, 0).
inline_op_code('$le_univ',		2,  9, predicate, 0).
inline_op_code('$ge_univ',		2, 10, predicate, 0).

inline_op_code('$eq',			2, 11, predicate, 0).
inline_op_code('$ineq',			2, 12, predicate, 0).
inline_op_code('$lt',			2, 13, predicate, 0).
inline_op_code('$gt',			2, 14, predicate, 0).
inline_op_code('$le',			2, 15, predicate, 0).
inline_op_code('$ge',			2, 16, predicate, 0).

inline_op_code('$var',			1, 17, predicate, 0).
inline_op_code('$atom',			1, 18, predicate, 0).
inline_op_code('$atomic',		1, 19, predicate, 0).
inline_op_code('$nonvar',		1, 20, predicate, 0).
inline_op_code('$number',		1, 21, predicate, 0).
inline_op_code('$float',		1, 22, predicate, 0).
inline_op_code('$integer',		1, 23, predicate, 0).
inline_op_code('$generic',		1, 24, predicate, 0).

inline_op_code('$plus',			3, 25, function,  1).
inline_op_code('$plus_1',		2, 26, function,  1).
inline_op_code('$minus',		3, 27, function,  1).
inline_op_code('$minus_1',		2, 28, function,  1).
inline_op_code('$times',		3, 29, function,  1).
inline_op_code('$div',			3, 30, function,  1).
inline_op_code('$intdiv',		3, 31, function,  1).
inline_op_code('$mod',			3, 32, function,  1).
inline_op_code('$tointeger',		2, 33, function,  1).
inline_op_code('$tofloat',		2, 34, function,  1).
inline_op_code('$floor',		2, 35, function,  1).
inline_op_code('$ceiling',		2, 36, function,  1).
inline_op_code('$truncate',		2, 37, function,  1).
inline_op_code('$round',		2, 38, function,  1).
inline_op_code('$eval_math',		2, 39, function,  1).
inline_op_code('$min',			3, 40, function,  1).
inline_op_code('$max',			3, 41, function,  1).
inline_op_code('$abs',			2, 42, function,  1).
inline_op_code('$neg',			2, 43, function,  1).
inline_op_code('$log',			2, 44, function,  1).
inline_op_code('$exp',			2, 45, function,  1).
inline_op_code('$pow',			3, 46, function,  1).
inline_op_code('$sqrt',			2, 47, function,  1).
inline_op_code('$cbrt',			2, 48, function,  1).

inline_op_code('$sin',			2, 49, function,  1).
inline_op_code('$cos',			2, 50, function,  1).
inline_op_code('$tan',			2, 51, function,  1).
inline_op_code('$asin',			2, 52, function,  1).
inline_op_code('$acos',			2, 53, function,  1).
inline_op_code('$atan',			2, 54, function,  1).

inline_op_code('$b_or',			3, 55, function,  1).
inline_op_code('$b_and',		3, 56, function,  1).
inline_op_code('$b_xor',		3, 57, function,  1).
inline_op_code('$b_not',		2, 58, function,  1).
inline_op_code('$lshift',		3, 59, function,  1).
inline_op_code('$rshift',		3, 60, function,  1).
inline_op_code('$msb',			2, 61, function,  1).
inline_op_code('$rng',			4, 62, function,  1).

inline_op_code('$aref',			3, 63, function,  1).

inline_op_code('$active',		1, 64, function,  1).

inline_op_code('$save',			1, 65, predicate, 0).

inline_op_code('$collect_plus',		1, 66, predicate, 0).
inline_op_code('$collect_times',	1, 67, predicate, 0).
