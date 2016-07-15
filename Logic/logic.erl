-module(logic).
-export([table/1]).
%  3.01 (**) Truth tables for logical expressions.\\
% Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2
% and equ/2 (for logical equivalence) which succeed or
% fail according to the result of their respective operations; e.g.
% and(A,B) will succeed, if and only if both A and B succeed.
% Note that A and B can be Prolog goals (not only the constants
% true and fail).
% A logical expression in two variables can then be written in 
% prefix notation, as in the following example: and(or(A,B),nand(A,B)).
% Now, write a predicate table/3 which prints the truth table of a
% given logical expression in two variables.

perms([]) 	-> [[]];
perms(L) 	-> [[H|T] || H <- L,
		   T <- perms(L -- [H])].

take_two([]) 		-> [];
take_two([Head|Tail]) 	-> [First,Second|_] = Head,
			   [[First,Second]] ++ take_two(Tail).

create_table() 	->  Liste = perms([true,true,false,false]),
		    NewListe = sets:from_list(take_two(Liste)),
		    sets:to_list(NewListe).

table(F) 	-> Table = create_table(),
		   table(F,Table).

%% Must be used with an anonymous function as:
%% 	fun(A,B) -> A and (A or B) end
table(_,[]) 		-> [];
table(F,[[A,B]|Tail]) 	-> [[A,B,F(A,B)]] ++ table(F,Tail).

%% 3.02 (*) Truth tables for logical expressions (2).
%% It works with the 3.01 method and anonymous function
