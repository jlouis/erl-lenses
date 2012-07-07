-module(lens).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% A dummy record which we use for doing the initial testing stuff on lenses.
-record(r, {a :: integer(),
            b :: integer() }).

%% A lens accessor for the 'a' parameter in the record. In due time,
%% these can be generated automatically by a parse transform
lens_a_access() ->
    {fun(#r { a = A}) -> A end,
     fun(X, R) -> R#r { a = X } end}.

%% Likewise for the 'b' parameter.
lens_b_access() ->
    {fun(#r { b = A }) -> A end,
     fun(X, R) -> R#r { b = X } end}.

%% In order to run EQC tests on this repository you need to have a way
%% to generate random 'r' records. This generator allows us to do
%% that, with integers as the contents.
gen_r() ->
    ?LET({A, B}, {int(), int()},
         #r { a = A, b = B}).

%% Lenses have three properties. If we get out a value and then
%% subsequently put it, nothing should change.
lens_prop_getput(Gen, Lens) ->
    {Get, Put} = Lens,
    ?FORALL(X, Gen,
            X == Put(Get(X), X)).

%% And if we put in a value and then get the value out, we should get
%% the value we just put.
lens_prop_putget(Gen, Val, Lens) ->
    {Get, Put} = Lens,
    ?FORALL({X, A}, {Gen, Val},
            A == Get(Put(A, X))).

%% The above two definitions makes for a well-behaved lens. A very
%% well behaved lens also satisfies this last rule: Last put wins and
%% behaves as if it was the only thing done to the lens.
lens_prop_putput(Gen, Val, Lens) ->
    {Get, Put} = Lens,
    ?FORALL({X, A, A1}, {Gen, Val, Val},
            Put(A1, Put(A, X)) == Put(A1, X)).

%% EQC properties for all of these
prop_t1() ->
    lens_prop_getput(gen_r(), lens_a_access()).

prop_t2() ->
    lens_prop_getput(gen_r(), lens_b_access()).

prop_t3() ->
    lens_prop_putget(gen_r(), int(), lens_a_access()).

prop_t4() ->
    lens_prop_putget(gen_r(), int(), lens_b_access()).

prop_t5() ->
    lens_prop_putput(gen_r(), int(), lens_a_access()).

prop_t6() ->
    lens_prop_putput(gen_r(), int(), lens_b_access()).

%% Test our code :)
t() ->
    eqc:module({numtests, 1000}, ?MODULE).

