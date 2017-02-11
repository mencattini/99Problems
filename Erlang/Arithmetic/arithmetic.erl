-module(arithmetic).
-export([is_prime/1,prime_factor/1,prime_factor_mult/1]).
-export([prime_lists/2,goldbach/1,goldbach_lists/2]).
-export([goldbach_lists/3,gcd/2,coprime/2]).
-export([toten_phi/1,phi/1]).

%% 2.01 (**) Determine whether a given integer number is prime.
is_prime(0) -> false;
is_prime(1) -> false;
is_prime(N) -> is_prime(N,N-1).

is_prime(_,1) 	-> true;
is_prime(N,Div) -> Divp = Div -1,
		   case N rem Div == 0 of
			  true 	-> false;
			  false -> is_prime(N,Divp)
		   end.  

%% 2.02 (**) Determine the prime factors of a given positive integer.
prime_factor(N) -> prime_factor(N,2).

prime_factor(1,_) 	-> [];
prime_factor(N,Factor) 	-> case is_prime(Factor) of
				false 	-> prime_factor(N,Factor+1);
				true 	-> case N rem Factor == 0 of
						true -> Np = N div Factor,
							[Factor|prime_factor(Np,Factor)];
						false -> prime_factor(N,Factor+1)
					   end
			   end.

%% 2.03 (**) Determine the prime factors of a given positive integer
prime_factor_mult(N) -> Liste = prime_factor(N),
			 mylists:encode(Liste).

%% 2.04 (*) A list of prime numbers.
prime_lists(Lower,Upper) when Upper == Lower
			      -> case is_prime(Lower) of
					 true -> [Lower];
					 false -> []
				 end;

prime_lists(Lower,Upper) when not(Upper == Lower)
			      -> case is_prime(Lower) of
					 true -> [Lower] ++ prime_lists(Lower+1,Upper);
					 false -> prime_lists(Lower+1,Upper)
				 end.

%% 2.05 (**) Goldbach's conjecture.
goldbach(N) -> Liste = prime_lists(0,N),
	       goldbach(N,Liste).

goldbach(_,[]) 		-> false;
goldbach(N,[Head|Tail]) -> case is_prime(N-Head) of
				   true  -> [Head,N-Head];
				   false -> goldbach(N,Tail)
			   end.

%% 2.06 (**) A list of Goldbach compositions.
goldbach_lists(Lower,Upper) when Lower == Upper
				 -> case Lower rem 2 == 0 of
					    true -> [goldbach(Lower)];
					    false -> []
				    end;

goldbach_lists(Lower,Upper) when not(Lower == Upper)
				 -> case Lower rem 2 == 0 of
					    true -> [goldbach(Lower)] ++ goldbach_lists(Lower+1,Upper);
					    false -> goldbach_lists(Lower+1,Upper)
				    end.

%% Second version, with upper born for adding in list
goldbach_lists(Lower,Upper,N) when Lower == Upper
				 -> case (Lower rem 2 == 0) and not(Lower == 2) of
					    true -> [First,Second] = goldbach(Lower),
						    if
							    First >= N ->
								    [[First,Second]];
							    First < N ->
								    []
						    end;
					    false -> []
				    end;

goldbach_lists(Lower,Upper,N) when not(Lower == Upper)
				 -> case (Lower rem 2 == 0) and not(Lower == 2) of
					    true -> [First,Second] = goldbach(Lower),
						    if
							    First >= N ->
								    [[First,Second]] ++ goldbach_lists(Lower+1,Upper,N);
							    First < N ->
								    goldbach_lists(Lower+1,Upper,N)
						    end;
					    false -> goldbach_lists(Lower+1,Upper,N)
				    end.

%% 2.07 (**) Determine the greatest common divisor of two positive integer numbers.
%%  with Euclid's method
gcd(A,B) when A < B  
	 -> gcd(B,A);

gcd(A,B) -> Reste = (A rem B),
	    case (Reste == 0) of
		true -> B;
		false -> gcd(B,Reste)
	    end.


%% 2.08 (*) Determine whether two positive integer numbers are coprime.
coprime(A,B) -> gcd(A,B) == 1.


%% 2.09 (**) Calculate Euler's totient function phi(m). 
toten_phi(N) 	-> length(toten_phi(N,N)).

toten_phi(_,M) when M == 1
		-> [1];

toten_phi(N,M) 	-> case (coprime(N,M)) of
			   true -> [M] ++ toten_phi(N,M-1);
			   false -> toten_phi(N,M-1)
		   end.

%% 2.10 (**) Calculate Euler's totient function phi(m) (2).
phi(M) -> Liste = prime_factor_mult(M),
	  round(eulerProduct(Liste)).

eulerProduct([]) 		-> 1;
eulerProduct([Head|Tail]) 	-> [M,P] = Head,
				   (P-1)*math:pow(P,M-1) * eulerProduct(Tail).


%% 2.11 (*) Compare the two methods of calculating Euler's totient function.
%% arithmetic:toten_phi(10000090). -> ~14s
%% arithmetic:phi(10000090). -> ~ 1s

