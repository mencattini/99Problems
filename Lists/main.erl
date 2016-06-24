-module(main).
-export([last/1,last_but_one/1,nth/2]).
-export([lengthMe/1,reverse/1,palindrome/1]).
-export([flatten/1,compress/1,pack/1]).
-export([encode/1,encode_modified/1]).

%% 1.01 (*) Find the last element of a list.
last([Element]) -> Element;
last([_|Tail]) -> last(Tail).

%% 1.02 (*) Find the last but one element of a list.
last_but_one([Element,_|[]]) -> Element;
last_but_one([_|Tail]) -> last_but_one(Tail).

%% 1.03 (*) Find the K'th element of a list.
nth(1,[Element|_]) -> Element;
nth(N,[_|Tail]) -> Np = N - 1, nth(Np, Tail).

%% 1.04 (*) Find the number of elements of a list.
lengthMe(Liste) -> lengthMe(0,Liste).
lengthMe(N,[]) -> N;
lengthMe(N,[_|Tail]) -> Np = N + 1, lengthMe(Np,Tail).

%% 1.05 (*) Reverse a list.
reverse(Liste) -> reverse(Liste,[]).

reverse([],Result) -> Result;
reverse([Head|Tail],Result) -> reverse(Tail,[Head|Result]).

%% 1.06 (*) Find out whether a list is a palindrome. 
palindrome(Liste) -> Liste == reverse(Liste).

%% 1.07 (**) Flatten a nested list structure.
flatten(Liste) -> lists:reverse(flatten(Liste,[])).

flatten([],Result) -> Result;
flatten([Head|Tail],Result) 
	when is_list(Head) ->	flatten(Tail,flatten(Head,Result)) ;

flatten([Head|Tail],Result) 
	when not(is_list(Head)) -> flatten(Tail,[Head|Result]).	

%% 1.08 (**) Eliminate consecutive duplicates of list elements.
compress(Liste) -> lists:reverse(compress(Liste,[])).						

compress([Head], Result) -> [Head|Result];
compress([Head,Head|Tail],Result) 
	when Head == Head -> compress([Head|Tail], Result);
compress([Head,Second|Tail],Result) -> compress([Second|Tail], [Head|Result]).

%% 1.09 (**) Pack consecutive duplicates of list elements into sublists.
pack(Liste) -> lists:reverse(pack(Liste,[])).

pack([],Result) -> Result;
pack(Liste,Result) -> 	[ResultTmp,NewTail] = small_pack(Liste,[]),
						pack(NewTail,[ResultTmp]++Result).

small_pack([Head],Result) -> [[Head|Result],[]];
small_pack([Head,Second|Tail],Result)
	when not(Head == Second) -> [[Head|Result],[Second|Tail]];
small_pack([Head,Head|Tail], Result)
	when Head == Head -> small_pack([Head|Tail],[Head|Result]).

% 1.10 (*) Run-length encoding of a list.
encode(Liste) -> 	NewListe = pack(Liste),
					lists:reverse(encode(NewListe,[])).

encode([],Result) -> Result;
encode([Head|Tail],Result) -> 	[First|_] = Head,
								Size = lengthMe(Head),
								encode(Tail,[[Size,First]|Result]).

%% 1.11 (*) Modified run-length encoding.
encode_modified(Liste) -> 	NewListe = encode(Liste),
							lists:reverse(encode_modified(NewListe,[])).

encode_modified([],Result) -> Result;
encode_modified([[Size,Element]|Tail],Result) 
	when Size == 1 -> encode_modified(Tail,[Element|Result]);
encode_modified([[Size,Element]|Tail],Result)
	when Size > 1 -> encode_modified(Tail,[[Size,Element]|Result]).