%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  byte-opcodes.pl
%
%


byte_opcode(Instruction, ByteCode) :-
	byte_op_code(Instruction, ByteCode), !.

byte_opcode(Instruction,_ByteCode) :- 
	error('unknown instruction (~w) lacks byte code', [Instruction]).



byte_op_code('Dummy_Instruction',	 0).
byte_op_code('Switch_On_Term',		 1).
byte_op_code('Switch_On_Constant',	 2).
byte_op_code('Switch_On_Structure',	 3).
byte_op_code('Try',			 4).
byte_op_code('Retry',			 5).
byte_op_code('Trust',			 6).
byte_op_code('Try_Me_Else',		 7).
byte_op_code('Retry_Me_Else',		 8).
byte_op_code('Trust_Me',		 9).
byte_op_code('Choice_X',		10).
byte_op_code('Choice_Y',		11).
byte_op_code('Cut',			12).
byte_op_code('Cut_X',			13).
byte_op_code('Cut_Y',			14).
byte_op_code('Inline',			15).
byte_op_code('Builtin',			16).
byte_op_code('Meta_Call',		17).
byte_op_code('Meta_Execute',		18).
byte_op_code('Require',			19).
byte_op_code('Require_Using',		20).
byte_op_code('Allocate',		21).
byte_op_code('Allocate2',		22).
byte_op_code('Deallocate',		23).
byte_op_code('Init',			24).
byte_op_code('Call',			25).
byte_op_code('Execute',			26).
byte_op_code('Proceed',			27).
byte_op_code('Fail',			28).

byte_op_code('Get_X_Variable',		29).
byte_op_code('Get_Y_Variable',		30).
byte_op_code('Get_Y_First_Value',	31).
byte_op_code('Get_X_Value',		32).
byte_op_code('Get_Y_Value',		33).
byte_op_code('Get_Constant',		34).
byte_op_code('Get_Nil',			35).
byte_op_code('Get_Structure',		36).
byte_op_code('Get_List',		37).
byte_op_code('Get_Constant_x0',		38).
byte_op_code('Get_Nil_X0',		39).
byte_op_code('Get_Structure_X0',	40).
byte_op_code('Get_List_X0',		41).

byte_op_code('Put_X_Void',		42).
byte_op_code('Put_Y_Void',		43).
byte_op_code('Put_X_Variable',		44).
byte_op_code('Put_Y_Variable',		45).
byte_op_code('Put_X_Value',		46).
byte_op_code('Put_Y_Value',		47).
byte_op_code('Put_X_Unsafe_Value',	48).
byte_op_code('Put_Y_Unsafe_Value',	49).
byte_op_code('Put_Constant',		50).
byte_op_code('Put_Nil',			51).
byte_op_code('Put_Structure',		52).
byte_op_code('Put_List',		53).

byte_op_code('Unify_Void',		54).
byte_op_code('Unify_X_Variable',	55).
byte_op_code('Unify_Y_Variable',	56).
byte_op_code('Unify_Y_First_Value',	57).
byte_op_code('Unify_X_Value',		58).
byte_op_code('Unify_Y_Value',		59).
byte_op_code('Unify_X_Local_Value',	60).
byte_op_code('Unify_Y_Local_Value',	61).
byte_op_code('Unify_Constant',		62).
byte_op_code('Unify_Nil',		63).
byte_op_code('Unify_Structure',		64).
byte_op_code('Unify_List',		65).


%%%
%%  Instructions used by the parallel machinery.
%
byte_op_code('Global_Get_X_Value',	66).
byte_op_code('Global_Get_Y_Value',	67).
byte_op_code('Global_Get_Constant',	68).
byte_op_code('Global_Get_Nil',		69).
byte_op_code('Global_Get_Structure',	70).
byte_op_code('Global_Get_List',		71).
byte_op_code('Global_Get_Constant_X0',	72).
byte_op_code('Global_Get_Nil_X0',	73).
byte_op_code('Global_Get_Structure_X0',	74).
byte_op_code('Global_Get_List_X0',	75).

byte_op_code('Global_Unify_X_Value',	76).
byte_op_code('Global_Unify_Y_Value',	77).
byte_op_code('Global_Unify_X_Local_Value',	78).
byte_op_code('Global_Unify_Y_Local_Value',	79).
byte_op_code('Global_Unify_Constant',	80).
byte_op_code('Global_Unify_Nil',	81).
byte_op_code('Global_Unify_Structure',	82).
byte_op_code('Global_Unify_List',	83).

byte_op_code('Build_Rec_Poslist',	84).
byte_op_code('Build_Poslist',		85).
byte_op_code('Build_Poslist_Value',	86).
byte_op_code('Build_Neglist',		87).
byte_op_code('Build_Neglist_Value',	88).
byte_op_code('Build_Variables',		89).

byte_op_code('Put_Nth_Head',		90).
byte_op_code('Put_Nth_Tail',		91).
byte_op_code('Put_Global_Arg',		92).
byte_op_code('Nil_Nth_Head',		93).

byte_op_code('Unify_Nth_Head',		94).
byte_op_code('Unify_Nth_Tail',		95).
byte_op_code('Unify_Global_Arg',	96).

byte_op_code('Start_Right_Body',	97).
byte_op_code('Start_Left_Body',		98).

byte_op_code('Initialize_Right',	99).
byte_op_code('Initialize_Left',		100).

byte_op_code('Spawn_Right',		101).
byte_op_code('Spawn_Left',		102).

byte_op_code('Await_Leftmost',		103).
byte_op_code('Await_Nonvar',		104).
byte_op_code('Await_Strictly_Nonvar',	105).
byte_op_code('Await_Variable',		106).

byte_op_code('Par_Builtin',		107).
byte_op_code('Par_Inline',		108).

byte_op_code('Lock_And_Get_Structure',	109).
byte_op_code('Lock_And_Get_List',	110).
byte_op_code('Unlock',			111).

byte_op_code('Jump',			112).


/*******************************************************************************
qload_code(X,Y) :- qload_byte_op_code(X,Y),!.
qload_code(X,_) :- 
	write(user_error,['inst has no qload opcode: ',X]),
	nl(user_error), !, fail.

qload_byte_code('qload_end_marker',	0).
qload_byte_code('qload_pred_start',	1).
qload_byte_code('qload_word',		2).
qload_byte_code('qload_n_bytecode',	3).
qload_byte_code('qload_def',		4).
qload_byte_code('qload_atom',		5).
qload_byte_code('qload_functor',	6).
qload_byte_code('qload_float',		7).
qload_byte_code('qload_integer',	8).
qload_byte_code('qload_start_table',	9).
qload_byte_code('qload_end_table',	10).
qload_byte_code('qload_pred_end',	11).
*******************************************************************************/