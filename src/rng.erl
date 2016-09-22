-module(rng).

-export([seed_rng/0, seed_rng/1, uniform_float/0]).

seed_rng() ->
	seed_rng(606418532).
	
seed_rng(Seed) -> 
	rand:seed(exs1024, {0, 0, Seed}).

uniform_float() ->
	rand:uniform().