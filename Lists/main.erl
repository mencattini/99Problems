-module(main).
-export([last/1,last_but_one/1,nth/2]).
-export([lengthMe/1,reverse/1,palindrome/1]).
-export([flatten/1]).

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
							