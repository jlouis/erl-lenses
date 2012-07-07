-module(lens).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-record(r, {a :: integer(),
            b :: integer() }).

gen_r() ->
    ?LET({A, B}, {int(), int()},
         #r { a = A, b = B}).

lens_a_access() ->
    {fun(#r { a = A}) -> A end,
     fun(X, R) -> R#r { a = X } end}.

lens_b_access() ->
    {fun(#r { b = A }) -> A end,
     fun(X, R) -> R#r { b = X } end}.

lens_prop_getput(Gen, Lens) ->
    {Get, Put} = Lens,
    ?FORALL(X, Gen,
            X == Put(Get(X), X)).

lens_prop_putget(Gen, Val, Lens) ->
    {Get, Put} = Lens,
    ?FORALL({X, A}, {Gen, Val},
            A == Get(Put(A, X))).

prop_t1() ->
    lens_prop_getput(gen_r(), lens_a_access()).

prop_t2() ->
    lens_prop_getput(gen_r(), lens_b_access()).

prop_t3() ->
    lens_prop_putget(gen_r(), int(), lens_a_access()).

prop_t4() ->
    lens_prop_putget(gen_r(), int(), lens_b_access()).

