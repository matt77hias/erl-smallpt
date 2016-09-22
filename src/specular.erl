-module(specular).

-export([reflectance0/2, schlick_reflectance/3, ideal_specular_reflect/2, ideal_specular_transmit/4]).

reflectance0(N1, N2) ->
	SR = (N1 - N2) / (N1 + N2),
	SR * SR.

schlick_reflectance(N1, N2, C) ->
	R0 = reflectance0(N1, N2),
	R0 + (1.0 - R0) * C * C * C * C * C.

ideal_specular_reflect(D, N) ->
	vector3:sub_v3(D, vector3:mul_v3(2.0 * vector3:dot_v3(N, D), N)).
	
ideal_specular_transmit(D, N, Nout, Nin) ->
	D_Re = ideal_specular_reflect(D, N),
	case (vector3:dot_v3(N, D) < 0) of
		true -> 	Out_to_In = true, 
					NL = N, 
					NN = Nout / Nin;
		false ->	Out_to_In = false, 
					NL = vector3:minus_v3(N), 
					NN = Nin / Nout
	end,
	CosTheta = vector3:dot_v3(D, NL),
    Cos2Phi = 1.0 - NN * NN * (1.0 - CosTheta * CosTheta),
	case (Cos2Phi < 0) of
		true ->		{D_Re, 1.0};
		false ->	
					D_Tr = vector3:normalize_v3(vector3:sub_v3(vector3:mul_v3(NN, D), vector3:mul_v3(NL, NN * CosTheta + math:sqrt(Cos2Phi)))),
					case Out_to_In of
						true ->		C = 1.0 + CosTheta;
						false ->	C = 1.0 - vector3:dot_v3(D_Tr, N)
					end,
					Re = schlick_reflectance(Nout, Nin, C),
					Pr_Re = 0.25 + 0.5 * Re,
					Random = rng:uniform_float(),
					case (Random < Pr_Re) of
						true -> 	{D_Re, Re / Pr_Re};
						false -> 	{D_Tr, (1.0 - Re) / (1.0 - Pr_Re)}
					end
	end.