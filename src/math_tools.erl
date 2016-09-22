-module(math_tools).

-export([pi/0, positive_infinity/0, floor/1, ceil/1, clamp/3, to_byte/2]).

pi() -> 3.14159265358979323846.
positive_infinity() -> 1.0e20.

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).
 
ceil(X) when X < 0 ->
    trunc(X);
ceil(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

clamp(X, Low, High) -> 
	case (X < Low) of
		true -> 	Low;
		false -> 	case (X > High) of
						true -> 	High;
						false -> 	X
					end
	end.

to_byte(X, Gamma) ->
	E = 1.0 / Gamma, 
	T = 255.0 * math:pow(X, E),
	trunc(clamp(T, 0.0, 255.0)).