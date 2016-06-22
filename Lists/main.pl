
%% 1.01 (*) Find the last element of a list.
last(Elem,[Elem]).
last(X,[_|Tail]) :- last(X,Tail).

%% 1.02 (*) Find the last but one element of a list.
last_but_one(Elem,[Elem,_|[]]).
last_but_one(X,[_ | Tail]) :- last_but_one(X,Tail).

%% 1.03 (*) Find the K'th element of a list.
nth(Element,[Element|_],1).
nth(Element,[_|Tail],N) :- 	Np is N -1 , 
							nth(Element,Tail,Np).

%% 1.04 (*) Find the number of elements of a list.
lengthMe([],0).
lengthMe([_|Tail],N) :- lengthMe(Tail, Np), 
						N is Np + 1.

%% 1.05 (*) Reverse a list.
reverse([Element], [Element]).
reverse([Head|Tail], Result) :- reverse(Tail,ResultTmp), 
								append(ResultTmp,[Head],Result).

%% 1.06 (*) Find out whether a list is a palindrome. 
palindrome(Liste) :- reverse(Liste,Liste).

%% 1.07 (**) Flatten a nested list structure.
flatten([],[]).
flatten([Head|Tail],Liste) :- 	is_list(Head), 
								flatten(Head,ResultTmp),
								flatten(Tail,Result),
								append(ResultTmp,Result,Liste).
flatten([Head|Tail],Liste) :-	not(is_list(Head)),
								flatten(Tail,Result),
								append([Head],Result,Liste).

%% 1.08 (**) Eliminate consecutive duplicates of list elements.
compress([Element],[Element]).
compress([Head,Head|Tail], Result) :- compress([Head|Tail],Result).
compress([Head,Second|Tail],Result) :- 	compress([Second|Tail],ResultTmp),
										append([Head],ResultTmp,Result).