-module(ray).

-export([eval_ray/2]).

eval_ray({Origin, Direction, _Tmin, _Tmax, _Depth}, T) ->
	vector3:add_v3(Origin, vector3:mul_v3(Direction, T)).