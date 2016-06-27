-module(arithmetic).
-compile(export_all).

%% 2.01 (**) Determine whether a given integer number is prime.
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
%%

prime_factors_mult(N) -> Liste = prime_factor(N),
			 mylists:encode(Liste).

