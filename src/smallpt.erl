-module(smallpt).

-export([main/0]).

% Scene
refractive_index_out() -> 1.0.
refractive_index_in() -> 1.5.
	
get_scene() ->
		[
        {1.0e5,	[100001, 40.8, 81.6], 	[0.0, 0.0, 0.0],  	[0.75,0.25,0.25], 		diffuse},
	    {1.0e5, [-99901, 40.8, 81.6], 	[0.0, 0.0, 0.0], 	[0.25,0.25,0.75], 		diffuse},
	    {1.0e5, [50.0, 40.8, 1.0e5],    [0.0, 0.0, 0.0], 	[0.75, 0.75, 0.75], 	diffuse},
	    {1.0e5, [50.0, 40.8, -99830],	[0.0, 0.0, 0.0],	[0.0, 0.0, 0.0], 		diffuse},
	    {1.0e5, [50.0, 1.0e5, 81.6],    [0.0, 0.0, 0.0], 	[0.75, 0.75, 0.75], 	diffuse},
	    {1.0e5, [50.0, -99918.4, 81.6], [0.0, 0.0, 0.0], 	[0.75, 0.75, 0.75], 	diffuse},
	    {16.5, 	[27.0, 16.5, 47.0],     [0.0, 0.0, 0.0], 	[0.999, 0.999, 0.999], 	specular},
	    {16.5,  [73.0, 16.5, 78.0],     [0.0, 0.0, 0.0], 	[0.999, 0.999, 0.999], 	refractive},
	    {600.0, [50.0, 681.33, 81.6],   [12.0, 12.0, 12.0],	[0.0, 0.0, 0.0],		diffuse}
		].

intersect_scene(Scene, Ray) ->
	intersect_scene(Scene, Ray, none, none).
intersect_scene([], _Ray, CTmax, CSphere) ->
	case (CSphere /= none) of
		true -> 	{true, CTmax, CSphere};
		false ->	false
	end;
intersect_scene([Head|Tail], {Origin, Direction, Tmin, Tmax, Depth}, CTmax, CSphere) ->
	case sphere:intersect_sphere(Head, {Origin, Direction, Tmin, Tmax, Depth}) of
		false 			-> intersect_scene(Tail, {Origin, Direction, Tmin, Tmax, Depth}, CTmax, CSphere);
		{SHit, Smax} 	-> intersect_scene(Tail, {Origin, Direction, Tmin, Smax, Depth}, Smax, Head)
	end.
	
radiance(Scene, {Origin, Direction, Tmin, Tmax, Depth}) ->
	case intersect_scene(Scene, {Origin, Direction, Tmin, Tmax, Depth}) of
		false ->	[0.0, 0.0, 0.0];
		{true, NTmax, {_Radius, Position, E, F, Reflection_t}} ->
					P = ray:eval_ray({Origin, Direction, Tmin, Tmax, Depth}, NTmax),
					N = vector3:normalize_v3(vector3:sub_v3(P, Position)),
					case (Depth > 4) of
						true ->		PrC = vector3:max_v3(F),
									case (rng:uniform_float() >= PrC) of
										true ->		NF0 = F,
													Quit = true;
										false ->	NF0 = vector3:div_v3(F, PrC),
													Quit = false
									end;
						false ->	NF0 = F,
									Quit = false
					end,
					case (Quit == true) of
						true ->		E;
						false ->	case Reflection_t of
										refractive -> 	{D, Pr1} = specular:ideal_specular_transmit(Direction, N, refractive_index_out(), refractive_index_in()),
														NF = vector3:mul_v3(NF0, Pr1);
										specular ->		D = specular:ideal_specular_reflect(Direction, N),
														NF = NF0;
										diffuse ->		NF = NF0,
														case (vector3:dot_v3(N, Direction) < 0) of
															true -> 	W = N;
															false -> 	W = vector3:minus_v3(N)
														end,
														case (abs(vector3:x_v3(W)) > 0.1) of
															true ->		U1 = [0.0, 1.0, 0.0];
															false ->	U1 = [1.0, 0.0, 0.0]
														end,
														U = vector3:normalize_v3(vector3:cross_v3(U1, W)),
														V = vector3:cross_v3(W, U),
														[Dx, Dy, Dz] = sampling:cosine_weighted_sample_on_hemisphere(rng:uniform_float(), rng:uniform_float()),
														D = vector3:normalize_v3(vector3:add_v3(vector3:mul_v3(Dz, W), vector3:add_v3(vector3:mul_v3(Dx, U), vector3:mul_v3(Dy, V))))
									end,
									L0 = radiance(Scene, {P, D, sphere:epsilon_sphere(), math_tools:positive_infinity(), Depth + 1}),
									vector3:add_v3(E, vector3:mul_v3(NF, L0))
					end
		end.

main() ->
	rng:seed_rng(),
	Width = 1024,
	Height = 768,
	Camera = get_camera(Width, Height),
	Scene = get_scene(),
	Args = init:get_plain_arguments(),
	Smax = get_samples(Args),
	Ls = loop_main(Scene, Camera, Height, Width, Smax),
	image_io:write_ppm(Width, Height, Ls).

get_samples([_|[Head|_]]) ->
	Head / 4;
get_samples(_) ->
	4.

get_camera(Width, Height) ->
	Eye = [50.0, 52.0, 295.6],
	Gaze = vector3:normalize_v3([0.0, -0.042612, -1.0]),
    Fov = 0.5135,
    Cx = [Width * Fov / Height, 0.0, 0.0],
	Cy = vector3:mul_v3(vector3:normalize_v3(vector3:cross_v3(Cx, Gaze)), Fov),
	{Eye, Cx, Cy, Gaze}.

loop_main(Scene, Camera, Height, Width, Smax) ->
	Ls = array:new([{size, Height * Width}, {fixed, true}, {default, [0.0, 0.0, 0.0]}]),
	loop_y(Scene, Camera, 0, Height, Width, Smax, Ls).
loop_y(_Scene, _Camera, Height, Height, _Width, _Smax, Ls) ->
	Ls;
loop_y(Scene, Camera, Y, Height, Width, Smax, Ls0) -> 
	io:format("\rRendering (~p spp) ~.2f%", [Smax * 4, 100.0 * Y / (Height - 1)]),
	Ls1 = loop_x(Scene, Camera, Y, Height, 0, Width, Smax, Ls0), 
	loop_y(Scene, Camera, Y + 1, Height, Width, Smax, Ls1).
loop_x(_Scene, _Camera, _Y, _Height, Width, Width, _Smax, Ls) ->
	Ls;
loop_x(Scene, Camera, Y, Height, X, Width, Smax, Ls0) ->
	Ls1 = loop_sy(Scene, Camera, Y, Height, X, Width, 0, 2, Smax, Ls0), 
	loop_x(Scene, Camera, Y, Height, X + 1, Width, Smax, Ls1).
loop_sy(_Scene, _Camera, _Y, _Height, _X, _Width, 2, 2, _Smax, Ls) ->
	Ls;
loop_sy(Scene, Camera, Y, Height, X, Width, Sy, 2, Smax, Ls0) ->
	Ls1 = loop_sx(Scene, Camera, Y, Height, X, Width, Sy, 0, 2, Smax, Ls0),
	loop_sy(Scene, Camera, Y, Height, X, Width, Sy + 1, 2, Smax, Ls1).
loop_sx(_Scene, _Camera, _Y, _Height, _X, _Width, _Sy, 2, 2, _Smax, Ls) ->
	Ls;
loop_sx(Scene, Camera, Y, Height, X, Width, Sy, Sx, 2, Smax, Ls0) ->
	L0 = loop_s(Scene, Camera, Y, Height, X, Width, Sy, Sx, 0, Smax, [0.0, 0.0, 0.0]),
	L1 = vector3:mul_v3(vector3:clamp_v3(L0, 0.0, 1.0), 0.25),
	Index = (Height - 1 - Y) * Width + X,
	Ls1 = array:set(Index, L1, Ls0),
	loop_sx(Scene, Camera, Y, Height, X, Width, Sy, Sx + 1, 2, Smax, Ls1).
loop_s(_Scene, _Camera, _Y, _Height, _X, _Width, _Sy, _Sx, Smax, Smax, L) ->
	L;
loop_s(Scene, Camera, Y, Height, X, Width, Sy, Sx, S, Smax, L0) ->
	L1 = do_s(Scene, Camera, Y, Height, X, Width, Sy, Sx, Smax, L0),
	loop_s(Scene, Camera, Y, Height, X, Width, Sy, Sx, S + 1, Smax, L1).
do_s(Scene, {Eye, Cx, Cy, Gaze}, Y, Height, X, Width, Sy, Sx, Smax, L) ->
	U1 = 2.0 * rng:uniform_float(),
	case (U1 < 1) of
		true -> 	Dx = math:sqrt(U1) - 1.0;
		false -> 	Dx = 1.0 - math:sqrt(2.0 - U1)
	end,
	Coefx = (((Sx + 0.5 + Dx) / 2.0 + X) / Width - 0.5),
	U2 = 2.0 * rng:uniform_float(),
	case (U2 < 1) of
		true -> 	Dy = math:sqrt(U2) - 1.0;
		false -> 	Dy = 1.0 - math:sqrt(2.0 - U2)
	end,
	Coefy = (((Sy + 0.5 + Dy) / 2.0 + Y) / Height - 0.5),
	Direction = vector3:add_v3(vector3:add_v3(vector3:mul_v3(Cx, Coefx), vector3:mul_v3(Cy, Coefy)), Gaze),
	NDirection = vector3:normalize_v3(Direction),
	NEye = vector3:add_v3(Eye, vector3:mul_v3(Direction, 130.0)),
	vector3:add_v3(L, vector3:div_v3(radiance(Scene, {NEye, NDirection, sphere:epsilon_sphere(), math_tools:positive_infinity(), 0}), Smax)).