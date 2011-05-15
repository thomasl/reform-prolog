%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  primitives.pl
%
%
% Analysis of primitives:
%
%   So far, only a limited number of primitives are considered. They will be
% greatly extended when time admits.
%
%

:- ensure_loaded(inlineable).


primitive(X) :- var(X), !,
	sys_error('primitive/1: var arg').

% Library operations

primitive(assert(_)) :- !.
primitive(asserta(_)) :- !.
primitive(assertz(_)) :- !.
primitive(retract(_)) :- !.
primitive(recorded(_,_,_)) :- !.
primitive(recorda(_,_,_)) :- !.
primitive(recordz(_,_,_)) :- !.
primitive(erase(_)) :- !.

primitive(read(_)) :- !.
primitive(read(_,_)) :- !.

primitive(length(_,_)) :- !.
primitive(sort(_,_)) :- !.
primitive(keysort(_,_)) :- !.
primitive(name(_,_)) :- !.
primitive(atom_chars(_,_)) :- !.
primitive(number_chars(_,_)) :- !.

primitive(statistics(_,_)) :- !.
primitive(statistics) :- !.

primitive(format(_,_)) :- !.
primitive(format(_,_,_)) :- !.

primitive(display(_)) :- !.

primitive(write(_)) :- !.
primitive(write(_,_)) :- !.
primitive(writeq(_)) :- !.
primitive(writeq(_,_)) :- !.
primitive(write_canonical(_)) :- !.
primitive(write_canonical(_,_)) :- !.

primitive(print(_)) :- !.
primitive(print(_,_)) :- !.

primitive(nl) :- !.
primitive(nl(_)) :- !.

primitive(open(_,_,_)) :- !.
primitive(close(_)) :- !.

primitive(absolute_file_name(_,_)) :- !.

primitive(raise_exception(_)) :- !.

primitive(copy_term(_,_)) :- !.
primitive(expand_term(_,_)) :- !.

primitive(random(_,_)) :- !.

primitive('$lifted_u'(_,_,_)) :- !.

primitive('$det') :- !.
primitive('$nondet') :- !.
primitive('$initially_det') :- !.
primitive('$initially_nondet') :- !.
primitive('$get_level'(_)) :- !.
primitive('$set_size'(_)) :- !.

primitive(X) :- inlineable(X), !.     % default: all primops!


meta_primitive(freeze(_,_)).
meta_primitive(call(_)).
meta_primitive(bagof(_,_,_)).
meta_primitive(setof(_,_,_)).
meta_primitive(findall(_,_,_)).
meta_primitive(on_exception(_,_,_)).

