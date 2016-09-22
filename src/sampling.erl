-module(sampling).

-export([uniform_sample_on_hemisphere/2, cosine_weighted_sample_on_hemisphere/2]).

uniform_sample_on_hemisphere(U1, U2) ->
	SinTheta = math:sqrt(max(0.0, 1.0 - U1 * U1)),
	Pi = math_tools:pi(), 
	Phi = 2.0 * Pi * U2,
	[math:cos(Phi) * SinTheta, math:sin(Phi) * SinTheta, U1]. 

cosine_weighted_sample_on_hemisphere(U1, U2) ->
	CosTheta = math:sqrt(1.0 - U1), 
	SinTheta = math:sqrt(U1),
	Pi = math_tools:pi(), 
	Phi = 2.0 * Pi * U2,
	[math:cos(Phi) * SinTheta, math:sin(Phi) * SinTheta, CosTheta].