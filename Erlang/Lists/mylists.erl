-module(mylists).
-compile(export_all).

%% 1.01 (*) Find the last element of a list.
last([Element]) -> Element;
last([_|Tail]) 	-> last(Tail).

%% 1.02 (*) Find the last but one element of a list.
last_but_one([Element,_|[]]) 	-> Element;
last_but_one([_|Tail]) 		-> last_but_one(Tail).

%% 1.03 (*) Find the K'th element of a list.
nth([],N) when N > 0 -> error;
nth(_,N) when N == 0 -> tooSmall;
nth([Element|_],N) when N == 1 -> Element;
nth([_|Reste],N) when N > 0 -> nth(Reste, N-1).

%% 1.04 (*) Find the number of elements of a list.
%% funny use of the previous function
my_length(Liste) -> my_length(Liste,1).

my_length(Liste,N) -> Res = nth(Liste,N),
											if
												Res == error ->
													N - 1;
												true -> % work as an 'else branch'
													my_length(Liste, N + 1)
											end.

%% 1.05 (*) Reverse a list.
reverse(Liste) -> reverse(Liste,[]).

reverse([],Result) 		-> Result;
reverse([Head|Tail],Result) 	-> reverse(Tail,[Head|Result]).

%% 1.06 (*) Find out whether a list is a palindrome. 
palindrome(Liste) -> Liste == reverse(Liste).

%% 1.07 (**) Flatten a nested list structure.
flatten(Liste) -> lists:reverse(flatten(Liste,[])).

flatten([],Result) 	-> Result;
flatten([Head|Tail],Result) when is_list(Head)
		 	-> flatten(Tail,flatten(Head,Result)) ;

flatten([Head|Tail],Result) when not(is_list(Head))
			-> flatten(Tail,[Head|Result]).	

%% 1.08 (**) Eliminate consecutive duplicates of list elements.
compress(Liste) -> lists:reverse(compress(Liste,[])).						

compress([Head], Result) 	-> [Head|Result];
compress([Head,Head|Tail],Result) when Head == Head
		       	       	-> compress([Head|Tail], Result);
compress([Head,Second|Tail],Result) when not(Head == Second)
				-> compress([Second|Tail], [Head|Result]).

%% 1.09 (**) Pack consecutive duplicates of list elements into sublists.
pack(Liste) -> lists:reverse(pack(Liste,[])).

pack([],Result) 	-> Result;
pack(Liste,Result) 	-> [ResultTmp,NewTail] = small_pack(Liste,[]),
			   pack(NewTail,[ResultTmp]++Result).

small_pack([Head],Result) 	-> [[Head|Result],[]];
small_pack([Head,Second|Tail],Result) when not(Head == Second)
				-> [[Head|Result],[Second|Tail]];
small_pack([Head,Head|Tail], Result) when Head == Head 
				-> small_pack([Head|Tail],[Head|Result]).

% 1.10 (*) Run-length encoding of a list.
encode(Liste) -> 	NewListe = pack(Liste),
			lists:reverse(encode(NewListe,[])).

encode([],Result) 		-> Result;
encode([Head|Tail],Result) 	-> [First|_] = Head,
				   Size = my_length(Head),
				   encode(Tail,[[Size,First]|Result]).

%% 1.11 (*) Modified run-length encoding.
encode_modified(Liste) -> 	NewListe = encode(Liste),
				lists:reverse(encode_modified(NewListe,[])).

encode_modified([],Result) 	-> Result;
encode_modified([[Size,Element]|Tail],Result) when Size == 1
				-> encode_modified(Tail,[Element|Result]);
encode_modified([[Size,Element]|Tail],Result) when Size > 1
				-> encode_modified(Tail,[[Size,Element]|Result]).

%% 1.12 (**) Decode a run-length encoded list.
decode(Liste) -> lists:reverse(decode(Liste,[])).

decode([],Result) 	-> Result;
decode([Head|Tail],Result) when is_list(Head) 
				-> [N,Element] = Head,
				   decode(Tail, 
				   getListe(N,Element,[]) ++ Result);
decode([Head|Tail],Result) when not(is_list(Head))
				-> decode(Tail,[Head|Result]).

getListe(0,_,Result) 		-> Result;
getListe(N,Element, Result) 	-> Np = N - 1,
				   getListe(Np, Element, [Element|Result]).

%% 1.13 (**) Run-length encoding of a list (direct solution).
encode_direct(Liste) -> lists:reverse(encode_direct(0,Liste,[])).

encode_direct(N,[Head],Result) 	-> Np = N + 1,
				   if 
					   Np == 1 ->
						   [Head] ++ Result;
					   Np > 1 ->
						   [[Np,Head]] ++ Result
				   end;
encode_direct(N,[Head,Head|Tail],Result) when Head == Head 
				-> Np = N + 1,
				   encode_direct(Np,[Head|Tail],Result);

encode_direct(N,[Head,Second|Tail],Result) when not(Head == Second) 
				-> Np = N + 1,
				   if
					   Np == 1 ->
						   encode_direct(0,[Second|Tail],[Head] ++ Result);
					   Np > 1 ->
						   encode_direct(0,[Second|Tail],[[Np,Head]] ++ Result)
				   end.

%% 1.14 (*) Duplicate the elements of a list.
dupli(Liste) -> lists:reverse(dupli(Liste,[])).

dupli([],Result) 		-> Result;
dupli([Head|Tail], Result) 	-> dupli(Tail,[Head,Head] ++ Result).

%% 1.15 (**) Duplicate the elements of a list a given number of times.
dupliN(Liste,N) -> lists:reverse(dupliN(N,Liste,[])).

dupliN(_,[],Result) 		-> Result;
dupliN(N,[Head|Tail],Result) 	-> dupliN(N,Tail, writeNTimes(N,Head,[]) ++ Result).

writeNTimes(0,_,Result) 	-> Result;
writeNTimes(N,Element,Result) 	-> Np = N - 1,
				 writeNTimes(Np,Element,[Element] ++ Result).

%% 1.16 (**) Drop every N'th element from a list.
drop(Liste,N) -> lists:reverse(drop(Liste,N,[])).

drop([_|Tail],1,Result) 	-> lists:reverse(Tail) ++ Result;
drop([Head|Tail],N,Result) 	-> Np = N -1,
				   drop(Tail,Np,[Head|Result]).

%% 1.17 (*) Split a list into two parts; the length of the first part is given.
split(Liste,N) -> split(Liste,N,[]).

split([Head|Tail],N,L1) when N == 1 
				-> [lists:reverse([Head|L1]),Tail];
split([Head|Tail],N,L1) 	-> Np = N - 1,
				   split(Tail,Np,[Head|L1]).

%% 1.18 (**) Extract a slice from a list.
slice(Liste,Inf,Sup) -> slice(Liste,Inf,Sup,1,[]).

slice(_,_,Sup,N,Result) when N > Sup
					-> lists:reverse(Result);
slice([_|Tail],Inf,Sup,N,Result) when N < Inf
				      	-> Np = N + 1,
					   slice(Tail,Inf,Sup,Np,Result);
slice([Head|Tail],Inf,Sup,N,Result) when N >= Inf, N =< Sup
					-> Np = N + 1,
					   slice(Tail,Inf,Sup,Np,[Head|Result]).

%% 1.19 (**) Rotate a list N places to the left.
rotate(Liste,Shift) when Shift >= 0 
			 -> rotate(Liste,Shift,[]);
rotate(Liste,Shift) when Shift < 0
			 -> lists:reverse(rotate(lists:reverse(Liste),-(Shift),[])).

rotate([Head|Tail],Shift,Result) when Shift =< 1
				      -> Tail ++ lists:reverse([Head|Result]);
rotate([Head|Tail],Shift,Result) when Shift > 1
				      -> Shiftp = Shift - 1,
					 rotate(Tail,Shiftp,[Head|Result]).

%% 1.20 (*) Remove the K'th element from a list.
remove_at(Liste,Pos) -> remove_at(Liste,Pos,[]).

remove_at([Head|Tail],Pos,Result) when Pos == 1
					-> [Head,lists:reverse(Result)++Tail];
remove_at([Head|Tail],Pos,Result) when Pos > 1
					-> Posp = Pos - 1,
					   remove_at(Tail,Posp,[Head|Result]).

%% 1.21 (*) Insert an element at a given position into a list.
insert_at(Element,Liste,Pos) -> insert_at(Element,Liste,Pos,[]).

insert_at(Element,Liste,Pos,Result) when Pos == 1
					-> lists:reverse([Element|Result]) ++ Liste;
insert_at(Element,[Head|Tail],Pos,Result) when Pos > 1
					-> Posp = Pos -1,
					   insert_at(Element,Tail,Posp,[Head|Result]).

%% 1.22 (*) Create a list containing all integers within a given range.
range(Inf,Sup) -> range(Inf,Sup,[]).

range(Inf,Sup,Result) when Inf == Sup
			-> lists:reverse([Inf|Result]);
range(Inf,Sup,Result) when Inf /= Sup
			-> Infp = Inf + 1,
			   range(Infp,Sup,[Inf|Result]).

%% 1.23 (**) Extract a given number of randomly selected elements from a list.
md_select(Liste,N) -> md_select(Liste,N,[]).

md_select(_,N,Result) when N == 0
			-> Result;
md_select(Liste,N,Result) when N > 0
			-> [Element,NewListe] = remove_at(Liste,random:uniform(length(Liste))),
			   Np = N - 1,
			   md_select(NewListe,Np,[Element|Result]).

%% 1.24 (*) Lotto: Draw N different random numbers from the set 1..M.
rnd_select(N,Max) -> Liste = range(1,Max),
		     md_select(Liste,N).

%% 1.25 (*) Generate a random permutation of the elements of a list.
rnd_permut(Liste) -> md_select(Liste,length(Liste)).

%% 1.26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

combination(N,Liste) -> combination(N,Liste,[]).

combination(N,Liste,Result) 	-> case length(Result) == (fac(length(Liste))/(fac(N)*fac(length(Liste)-N))) of
					true -> Result;
					false -> SubListe = md_select(Liste,N),
						 case not(lists:member(SubListe,Result)) of
							true -> combination(N,Liste,[SubListe|Result]);
							false -> combination(N,Liste,Result)
						 end
				   end.

%% 1.27 (**) Group the elements of a set into disjoint subsets.

select(Liste,N) -> select(Liste,N,[]).

select(Liste,N,Result) when N == 0
			-> [Result,Liste];
select(Liste,N,Result) when N > 0
			-> [Element,NewListe] = remove_at(Liste,random:uniform(length(Liste))),
			   Np = N - 1,
			   select(NewListe,Np,[Element|Result]).

group3(Liste) -> group3(Liste,[]).

group3(Liste,Result) -> case length(Result) == (fac(length(Liste))/(fac(3)*fac(2)*fac(4)*fac(length(Liste)-2-3-4))) of
				true -> Result;
				false -> [SubListe1,NewListe] = select(Liste,2),
					 [SubListe2,NewNewListe] = select(NewListe,3),
					 [SubListe3,_] = select(NewNewListe,4),
					 Res = [lists:sort(SubListe1),lists:sort(SubListe2),lists:sort(SubListe3)],
					 case not(lists:member(Res,Result)) of
						true -> group3(Liste,[Res|Result]);
						false -> group3(Liste,Result)
					end
				end.

%% Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

multiply(Liste) -> multiply(Liste,1).

multiply([],Result) 		-> Result;
multiply([Head|Tail],Result) 	-> Resultp = Result * Head,
				   multiply(Tail,Resultp).

listSelect(_,[],Result) 		-> Result;
listSelect(Liste,[Head|Tail],Result) 	-> [SubListe,NewListe] = select(Liste,Head),
					   listSelect(NewListe,Tail,[lists:sort(SubListe)|Result]).

group(Liste,ListeSize) 		-> group(Liste,ListeSize,[]).

group(Liste,ListeSize,Result) 	-> case length(Result) == (fac(length(Liste))/multiply(lists:map(fun(L) -> fac(L) end,ListeSize))) of
	true -> length(Result);
	false -> Res = listSelect(Liste,ListeSize,[]),
		 case not(lists:member(Res,Result)) of
			 true -> group(Liste,ListeSize,[Res|Result]);
			 false -> group(Liste,ListeSize,Result)
		 end
	end.

%% 1.28 (**) Sorting a list of lists according to length of sublists

quickSort([]) 		-> [];
quickSort([Head|Tail]) 	-> quickSort([E || E <- Tail, length(E) < length(Head)]) ++
		 	   [Head] ++
			   quickSort([E || E <- Tail, length(E) >= length(Head)]).

lsort(Liste) -> quickSort(Liste).


