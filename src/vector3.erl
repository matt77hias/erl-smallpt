-module(vector3).

-export([	x_v3/1, y_v3/1, z_v3/1, get_v3/2, 
			minus_v3/1, add_v3/2, sub_v3/2, mul_v3/2, div_v3/2, dot_v3/2, cross_v3/2, 
			eq_v3/2, ne_v3/2, le_v3/2, lt_v3/2, ge_v3/2, gt_v3/2, mind_v3/1, maxd_v3/1, min_v3/1, max_v3/1,
			sqrt_v3/1, pow_v3/2, abs_v3/1, min_v3/2, max_v3/2, round_v3/1, floor_v3/1, ceil_v3/1, trunc_v3/1, clamp_v3/3, lerp_v3/3, permute_v3/4,
			norm2s_v3/1, norm2_v3/1, normalize_v3/1
		]).

x_v3([Vx, _, _]) -> Vx.
y_v3([_, Vy, _]) -> Vy.
z_v3([_, _, Vz]) -> Vz.

get_v3([Vx, _, _], 0) -> Vx;
get_v3([_, Vy, _], 1) -> Vy;
get_v3([_, _, Vz], 2) -> Vz.

minus_v3([Vx, Vy, Vz]) -> [-Vx, -Vy, -Vz].
add_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> [V1x + V2x, V1y + V2y, V1z + V2z];
add_v3([Vx, Vy, Vz], A) -> [Vx + A, Vy + A, Vz + A];
add_v3(A, [Vx, Vy, Vz]) -> [A + Vx, A + Vy, A + Vz].
sub_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> [V1x - V2x, V1y - V2y, V1z - V2z];
sub_v3([Vx, Vy, Vz], A) -> [Vx - A, Vy - A, Vz - A];
sub_v3(A, [Vx, Vy, Vz]) -> [A - Vx, A - Vy, A - Vz].
mul_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> [V1x * V2x, V1y * V2y, V1z * V2z];
mul_v3([Vx, Vy, Vz], A) -> [Vx * A, Vy * A, Vz * A];
mul_v3(A, [Vx, Vy, Vz]) -> [A * Vx, A * Vy, A * Vz].	
div_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> [V1x / V2x, V1y / V2y, V1z / V2z];
div_v3([Vx, Vy, Vz], A) -> IA = 1.0 / A, [Vx * IA, Vy * IA, Vz * IA];
div_v3(A, [Vx, Vy, Vz]) -> [A / Vx, A / Vy, A / Vz].
	
dot_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> V1x * V2x + V1y * V2y + V1z * V2z.
cross_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> [V1y * V2z - V1z * V2y, V1z * V2x - V1x * V2z, V1x * V2y - V1y * V2x].
	
eq_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> (V1x == V2x) and (V1y == V2y) and (V1z == V2z).
ne_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> (V1x /= V2x) or (V1y /= V2y) or (V1z /= V2z).
le_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> (V1x =< V2x) and (V1y =< V2y) and (V1z =< V2z).
lt_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> (V1x < V2x) and (V1y < V2y) and (V1z < V2z).
ge_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> (V1x >= V2x) and (V1y >= V2y) and (V1z >= V2z).
gt_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> (V1x > V2x) and (V1y > V2y) and (V1z > V2z).
	
mind_v3([Vx, Vy, Vz]) -> 
	case ((Vx < Vy) and (Vx < Vz)) of
		true -> 	0;
		false ->	case (Vy < Vz) of
						true -> 	1;
						false -> 	2
					end
	end.
maxd_v3([Vx, Vy, Vz]) -> 
	case ((Vx > Vy) and (Vx > Vz)) of
		true -> 	0;
		false ->	case (Vy > Vz) of
						true -> 	1;
						false -> 	2
					end
	end.

min_v3([Vx, Vy, Vz]) -> min(min(Vx, Vy), Vz).
max_v3([Vx, Vy, Vz]) -> max(max(Vx, Vy), Vz).
	
sqrt_v3([Vx, Vy, Vz]) -> [math:sqrt(Vx), math:sqrt(Vy), math:sqrt(Vz)].
pow_v3([Vx, Vy, Vz], E) -> [math:pow(Vx, E), math:pow(Vy, E), math:pow(Vz, E)].
abs_v3([Vx, Vy, Vz]) -> [abs(Vx), abs(Vy), abs(Vz)].
min_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> [min(V1x, V2x), min(V1y, V2y), min(V1z, V2z)].
max_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) -> [max(V1x, V2x), max(V1y, V2y), max(V1z, V2z)].
round_v3([Vx, Vy, Vz]) -> [round(Vx), round(Vy), round(Vz)].
floor_v3([Vx, Vy, Vz]) -> [math_tools:floor(Vx), math_tools:floor(Vy), math_tools:floor(Vz)].
ceil_v3([Vx, Vy, Vz]) -> [math_tools:ceil(Vx), math_tools:ceil(Vy), math_tools:ceil(Vz)].
trunc_v3([Vx, Vy, Vz]) -> [trunc(Vx), trunc(Vy), trunc(Vz)].
clamp_v3([Vx, Vy, Vz], Low, High) -> [math_tools:clamp(Vx, Low, High), math_tools:clamp(Vy, Low, High), math_tools:clamp(Vz, Low, High)].
lerp_v3(A, V1, V2) -> add_v3(V1, mul_v3(sub_v3(V2, V1), A)).
permute_v3(V, X, Y, Z) -> [get_v3(V, X), get_v3(V, Y), get_v3(V, Z)].

norm2s_v3([Vx, Vy, Vz]) -> Vx * Vx + Vy * Vy + Vz * Vz.
norm2_v3([Vx, Vy, Vz]) -> math:sqrt(norm2s_v3([Vx, Vy, Vz])).
normalize_v3([Vx, Vy, Vz]) -> div_v3([Vx, Vy, Vz], norm2_v3([Vx, Vy, Vz])).