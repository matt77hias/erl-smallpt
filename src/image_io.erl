-module(image_io).

-export([gamma/0, write_ppm/3, write_ppm/4]).

gamma() -> 2.2.

write_ppm(Width, Height, Ls) ->
	write_ppm(Width, Height, Ls, "erl-image.ppm").
write_ppm(Width, Height, Ls, Fname) -> 
	{ok, Stream} = file:open(Fname, [write]),
	io:format(Stream, "P3~n~p ~p~n255~n", [Width, Height]),
	write_ppm_Ls(Stream, array:to_list(Ls)),
    file:close(Stream).
	
write_ppm_Ls(Stream, [Head | []]) ->
	write_ppm_L(Stream, Head);
write_ppm_Ls(Stream, [Head | Tail]) ->
	write_ppm_L(Stream, Head),
	file:write(Stream, " "),
	write_ppm_Ls(Stream, Tail);
write_ppm_Ls(_, []) -> 
	void.
	
write_ppm_L(Stream, [Lx, Ly, Lz]) ->
	Gamma = gamma(),
	io:format(Stream, "~p ~p ~p", [math_tools:to_byte(Lx, Gamma), math_tools:to_byte(Ly, Gamma), math_tools:to_byte(Lz, Gamma)]).