/* BENCH.PL : The classic Prolog benchmark

        Supplied by Quintus Computer Systems, Inc.
        April 30th 1984
*/

/* ======================================================================
   This benchmark gives the raw speed of a Prolog system.

   The measure of logical inferences per second (Lips) used here is taken to
   be procedure calls per second over an example with not very complex
   procedure calls. The example used is that of "naive reversing" a list,
   which is an expensive, and therefore stupid, way of reversing a list.  It
   does, however, produce a lot of procedure calls. (In theoretical terms,
   this algorithm is O(n^2) on the length of the list).
*/

get_cpu_time(T) :- statistics(runtime,[T,_]).   /* Quintus Prolog version */

/* ----------------------------------------------------------------------
        nrev(L1,L2)      -- L2 is the list L1 reversed.
        append(L1,L2,L3) -- L1 appended to L2 is L3.
        data(L)          -- L is a thirty element list.

        This is the program executed by the benchmark.
        It is called "naive reverse" because it is a very expensive way
        of reversing a list. Its advantage, for our purposes, is that
        it generates a lot of procedure calls. To reverse a thirty element
        list requires 496 Prolog procedure calls.
   ---------------------------------------------------------------------- */

nrev([],[]).
nrev([X|Rest],Ans) :- nrev(Rest,L), nappend(L,Ans,[X]).

nappend([],L,L).
nappend([X|L1],[X|L3],L2) :- nappend(L1,L3,L2).

data([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
        21,22,23,24,25,26,27,28,29,30]).


lots :-
        eg_count(Count),
        bench(Count),
        fail.
lots.

eg_count(50).
eg_count(100).
eg_count(200).
/*
eg_count(500).
eg_count(1000).
*/

bench(Count) :-
        get_cpu_time(T0),
        dodummy(Count),
        get_cpu_time(T1),
        dobench(Count),
        get_cpu_time(T2),
        report(Count,T0,T1,T2).


dobench(Count) :-
        data(List),
        repeat(Count),
        nrev(List,_),
        fail.
dobench(_).


dodummy(Count) :-
        data(List),
        repeat(Count),
        dummy(List,_),
        fail.
dodummy(_).

dummy(_,_).

repeat(_N).
repeat(N) :- N > 1, N1 is N-1, repeat(N1).      

report(Count,T0,T1,T2) :-
        Time1 is T1-T0,
        Time2 is T2-T1,
        Time  is Time2-Time1,           /* Time spent on nreving lists */
        calculate_lips(Count,Time,Lips,Units),
        nl,
        write(Lips), write(' lips for '), write(Count),
        write(' iterations taking '), write(Time),
        write(' '), write(Units), write(' ('),
        write(Time2-Time1), write(')'),
        nl.



calculate_lips(_Count,Time,Lips,'msecs') :-     /* Time can be 0 for small */
        Time is 0, !, Lips is 0.                /* values of Count!        */
calculate_lips(Count,Time,Lips,'msecs') :- 
        Lips  is (496*float(Count)*1000)/Time.

/*
        % get_constant(msecs,3)
        mov(tagged(msecs),u(1)),
        movcc(3,u(0)),
        jmpl(kernel(get_constant),kl),
        % function_1(5,u(1),0,128,[put_constant(496,u(0)),function_2(36,u(0),u(0),u(1),128,_890),put_constant(1000,u(1)),function_2(36,u(0),u(0),u(1),128,_916),function_2(37,u(0),u(0),1,128,_953),get_x_value(2,u(0)),proceed],[])
        movcc(0,u(0)),
        jmpl(kernel(f1_float),kl),
        word(integer(128)),
        word(maximum_x([u(1),1,2])),
        mov(u(0),u(1)),
        % get_live_ARs([u(1),1,2])
        % put_constant(496,u(0))
        movcc(tagged(496),u(0)),
        % function_2(36,u(0),u(0),u(1),128,[put_constant(1000,u(1)),function_2(36,u(0),u(0),u(1),128,_916),function_2(37,u(0),u(0),1,128,_953),get_x_value(2,u(0)),proceed],[])
        jmpl(kernel(f2_times),kl),
        word(integer(128)),
        word(maximum_x([u(0),1,2])),
        % get_live_ARs([u(0),1,2])
        % put_constant(1000,u(1))
        mov(tagged(1000),u(1)),
        % function_2(36,u(0),u(0),u(1),128,[function_2(37,u(0),u(0),1,128,_953),get_x_value(2,u(0)),proceed],[])
        tst(u(0)),
        jmpl(kernel(f2_times),kl),
        word(integer(128)),
        word(maximum_x([u(0),1,2])),
        % get_live_ARs([u(0),1,2])
        % function_2(37,u(0),u(0),1,128,[get_x_value(2,u(0)),proceed],[])
        mov(1,u(1)),
        tst(u(0)),
        jmpl(kernel(f2_fdivide),kl),
        word(integer(128)),
        word(maximum_x([u(0),2])),
        % get_live_ARs([u(0),2])
        % get_x_value(2,u(0))
        mov(2,u(1)),
        mov(l,kl),
        tst(u(0)),
        jmp(kernel(get_value)),
        % end_chunk
        % proceed

_1216:
        % get_constant(msecs,3)
        insn(sethi,i,i(hi(tagged(msecs))),r(9,u(1))),
        insn(or,r(9,u(1)),i(lo(tagged(msecs))),r(9,u(1))),
        jmpl(kernel(get_constant),r(15,kl)),
        orcc(r(0,g0),r(19,x(3)),r(8,u(0))),
        % function_1(5,u(1),0,128,[put_constant(496,u(0)),function_2(36,u(0),u(0),u(1),128,_890),put_constant(1000,u(1)),function_2(36,u(0),u(0),u(1),128,_916),function_2(37,u(0),u(0),1,128,_953),get_x_value(2,u(0)),proceed],[])
        jmpl(kernel(f1_float),r(15,kl)),
        orcc(r(0,g0),r(16,x(0)),r(8,u(0))),
        word(i(128)),
        word(i(2)),
        % get_live_ARs([u(1),1,2])
        % put_constant(496,u(0))
        or(r(0,g0),r(8,u(0)),r(9,u(1))),
        insn(sethi,i,i(hi(tagged(496))),r(11,t(0))),
        insn(or,r(11,t(0)),i(lo(tagged(496))),r(11,t(0))),
        % function_2(36,u(0),u(0),u(1),128,[put_constant(1000,u(1)),function_2(36,u(0),u(0),u(1),128,_916),function_2(37,u(0),u(0),1,128,_953),get_x_value(2,u(0)),proceed],[])
        jmpl(kernel(f2_times),r(15,kl)),
        orcc(r(0,g0),r(11,t(0)),r(8,u(0))),
        word(i(128)),
        % get_live_ARs([u(0),1,2])
        % put_constant(1000,u(1))
        word(i(2)),
        insn(sethi,i,i(hi(tagged(1000))),r(9,u(1))),
        % function_2(36,u(0),u(0),u(1),128,[function_2(37,u(0),u(0),1,128,_953),get_x_value(2,u(0)),proceed],[])
        insn(or,r(9,u(1)),i(lo(tagged(1000))),r(9,u(1))),
        jmpl(kernel(f2_times),r(15,kl)),
        orcc(r(0,g0),r(8,u(0)),r(0,g0)),
        word(i(128)),
        % get_live_ARs([u(0),1,2])
        % function_2(37,u(0),u(0),1,128,[get_x_value(2,u(0)),proceed],[])
        word(i(2)),
        or(r(0,g0),r(17,x(1)),r(9,u(1))),
        jmpl(kernel(f2_fdivide),r(15,kl)),
        orcc(r(0,g0),r(8,u(0)),r(0,g0)),
        word(i(128)),
        % get_live_ARs([u(0),2])
        % get_x_value(2,u(0))
        word(i(2)),
        or(r(0,g0),r(18,x(2)),r(9,u(1))),
        orcc(r(0,g0),r(8,u(0)),r(0,g0)),
        jmp(kernel(get_value)),
        sub(r(29,l),i(8),r(15,kl)),
        % end_chunk
        % proceed

        short(240),
        short(0),
        0x10011000000000000000000000000,
        add_hi_tagged(msecs),
        0x10010010000100100110000000000000,
        add_lo_tagged(msecs),
        call_builtin(98),
        0x10010000100100000000000000010011,
        call_builtin(176),
        0x10010000100100000000000000010000,
        0x10000000,
        0x10,
        0x10010010000100000000000000001000,
        0x10111000000000000000000000000,
        add_hi_tagged(496),
        0x10010110000100101110000000000000,
        add_lo_tagged(496),
        call_builtin(180),
        0x10010000100100000000000000001011,
        0x10000000,
        0x10,
        0x10011000000000000000000000000,
        add_hi_tagged(1000),
        0x10010010000100100110000000000000,
        add_lo_tagged(1000),
        call_builtin(180),
        0x10000000100100000000000000001000,
        0x10000000,
        0x10,
        0x10010010000100000000000000010001,
        call_builtin(183),
        0x10000000100100000000000000001000,
        0x10000000,
        0x10,
        0x10010010000100000000000000010010,
        0x10000000100100000000000000001000,
        call_builtin(96),
        0x10011110001001110110000000001000,

Size: 124
SIGSEGV 11: segmentation violation
stopped at      _prolog_make_native_code_object+0x17a8:         ld      [%o2 + %o1], %o1
$r
g0    0x0       __DYNAMIC               l0      0x9ce68         _self
g1    0x78000000                        l1      0x1a0234
g2    0x8000000                         l2      0xa01001e4
g3    0xffffffff                        l3      0x9ce68         _self
g4    0x20      __DYNAMIC+0x20          l4      0x40a0ff8
g5    0xe0000000                        l5      0xe40a1000
g6    0x0       __DYNAMIC               l6      0x40a0ff8
g7    0x0       __DYNAMIC               l7      0xfffffffa
o0    0x1a0238                          i0      0x9ce68         _self
o1    0x7c09fba0                        i1      0x27fc8         _prolog_make_native_code_object
o2    0x9d7d8   _builtintab             i2      0x3236c         _fu1_minus
o3    0xe40a0b68                        i3      0x40a101c
o4    0x2b7     __DYNAMIC+0x2b7         i4      0x3             __DYNAMIC+0x3
o5    0xf81528f4                        i5      0x0             __DYNAMIC
sp    0xf7fff4f8                        fp      0xf7fff6a8
o7    0x282c4   _prolog_make_native_code_object+0x2fc   i7  0x6df8             _wam+0x15a4
y     0x50000000
psr   0x401084
pc    0x29770   _prolog_make_native_code_object+0x17a8
npc   0x29774   _prolog_make_native_code_object+0x17ac
*/
