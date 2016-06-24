
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

%% 1.09 (**) Pack consecutive duplicates of list elements into sublists.
pack([],[]).
pack([Head,Head|Tail],Result):- small_pack([Head,Head|Tail],SubList,NewTail),
								append([SubList],ResultTmp,Result),
								pack(NewTail,ResultTmp).

pack([Head|Tail],Result) :- 	pack(Tail,ResultTmp),
								append([[Head]],ResultTmp,Result).


small_pack([Head],[Head],[]).
small_pack([Head,Second|Tail],[Head],[Second|Tail]) :- not(Head = Second).
small_pack([Head,Head|Tail], Result, NewTail) :- 	append([Head],ResultTmp,Result),
													small_pack([Head|Tail],ResultTmp, NewTail).
												
%% 1.10 (*) Run-length encoding of a list.
encode(Liste,Result) :- pack(Liste,ResultTmp),
						small_encode(ResultTmp,Result).

small_encode([],[]).
small_encode([Head|Tail],Result) :- [First|_] = Head,
									lengthMe(Head,Size),
									Liste = [Size,First],
									append([Liste],ResultTmp,Result),
									small_encode(Tail,ResultTmp).

%% 1.11 (*) Modified run-length encoding.
encode_modified(Liste,Result) :- 	encode(Liste,ResultTmp),
									small_modified_encode(ResultTmp,Result).

small_modified_encode([],[]).
small_modified_encode([Head|Tail],Result) :- 	[Size,Element] = Head,
												Size = 1,
												append([Element],ResultTmp,Result),
												small_modified_encode(Tail,ResultTmp).
small_modified_encode([Head|Tail],Result) :- 	[Size,_] = Head,
												not(Size = 1),
												append([Head],ResultTmp,Result),
												small_modified_encode(Tail,ResultTmp).												