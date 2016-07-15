-module(logic).
-export([table/1,table_general/2]).
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

take([],_) 		-> [];
take([Head|Tail],N) 	-> Liste = lists:sublist(Head,N),
			   [Liste] ++ take(Tail,N).

create_table(N)	->  True = lists:duplicate(N,true),
		    False = lists:duplicate(N,false),
		    Liste = perms(True ++ False),
		    NewListe = sets:from_list(take(Liste,N)),
		    sets:to_list(NewListe).

table(F) 	-> Table = create_table(2),
		   table(F,Table).

%% Must be used with an anonymous function as:
%% 	fun([A,B]) -> A and (A or B) end
table(_,[]) 		-> [];
table(F,[Head|Tail]) 	-> [Head ++ [F(Head)]] ++ table(F,Tail).

%% 3.02 (*) Truth tables for logical expressions (2).
%% It works with the 3.01 method and anonymous function


%% 3.03 (**) Truth tables for logical expressions (3). 
table_general(N,F) 	-> Table = create_table(N),
			   table(F,Table).
