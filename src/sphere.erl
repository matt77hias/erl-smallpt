-module(sphere).

-export([epsilon_sphere/0, intersect_sphere/2]).

epsilon_sphere() -> 1.0e-4.

intersect_sphere({Radius, Position, _E, _F, _Reflection_t}, {Origin, Direction, Tmin, Tmax, _Depth}) ->
	OP = vector3:sub_v3(Position, Origin),
	DOP = vector3:dot_v3(Direction, OP),
	Discriminant = DOP * DOP - vector3:dot_v3(OP, OP) + Radius * Radius,
	case Discriminant < 0 of
		true -> 	false;
		false ->	SDiscriminant = math:sqrt(Discriminant),
					Smin = DOP - SDiscriminant,
					case ((Tmin < Smin) and (Smin < Tmax)) of
						true -> 	{true, Smin};
						false ->	Smax = DOP + SDiscriminant,
									case ((Tmin < Smax) and (Smax < Tmax)) of
										true -> 	{true, Smax};
										false -> 	false
									end
					end
	end.