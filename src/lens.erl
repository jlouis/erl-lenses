-module(lens).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% This code base example is a "Functional Lens Library" for Erlang.
%% The idea of a Lens is to provide an abstraction which is
%% * like an accessor/mutator pair in imperative OO languages, but
%% * Is functional and algebraically composable
%%
%% This file documents lenses and their use in Erlang. The hope is
%% that over time this will develop into a library for lenses in
%% Erlang, even though what I will be able to do at spawn(fun() ->
%% fest(2012) end) will be limited in extent.



%% A dummy record which we use for doing the initial testing stuff on lenses.
-record(r, {a :: integer(),
            b :: integer() }).

%% A lens accessor for the 'a' parameter in the record. In due time,
%% these can be generated automatically by a parse transform
%% Likewise for the 'b' parameter.
lens_r_naive(a) -> {fun(#r { a = A}) -> A end,
                    fun(X, R) -> R#r { a = X } end};
lens_r_naive(b) -> {fun(#r { b = A }) -> A end,
                    fun(X, R) -> R#r { b = X } end}.

%% This will get tedious in the long run...
access_e(N) ->
    {fun(R) -> element(N, R) end,
     fun(A, R) -> setelement(N, R, A) end}.

lens_r(a) -> access_e(#r.a);
lens_r(b) -> access_e(#r.b).

%% Haskell already has lenses. This is the running example of deeper
%% importance. We have kittens and kittens have color.
%% All kittens start out as black kittens. Like Ford would have wanted it.
-record(color, {r = 0.0 :: float(),
                g = 0.0 :: float(),
                b = 0.0 :: float() }).

-record(kitten, {name :: string(),
                 color :: #color{} }).

%% We manually derive the needed lenses, but first a helper
lens_color(r) -> access_e(#color.r);
lens_color(g) -> access_e(#color.g);
lens_color(b) -> access_e(#color.b).

lens_kitten(name) -> access_e(#kitten.name);
lens_kitten(color) -> access_e(#kitten.color).

%% In order to run EQC tests on this repository you need to have a way
%% to generate random 'r' records. This generator allows us to do
%% that, with integers as the contents.
gen_r() ->
    ?LET({A, B}, {int(), int()},
         #r { a = A, b = B}).

%% Our naive lenses behave as our accessor lenses
prop_naive_access_e() ->
    ?FORALL({R, I}, {gen_r(), int()},
            begin
                {GetNA, PutNA} = lens_r_naive(a),
                {GetNB, PutNB} = lens_r_naive(b),
                {GetA,  PutA}  = lens_r(a),
                {GetB,  PutB}  = lens_r(b),
                
                lists:all([GetNA(R) == GetA(R),
                           GetNB(R) == GetB(R),
                           PutNA(R, I) == PutA(R, I),
                           PutNB(R, I) == PutB(R, I)])
            end).

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

%% The point is that we would like to *PROVE* the above properties for
%% the lenses we derive. But proofs are large, boring and hard. Hence,
%% we simply provide a tool which can test our lens access via
%% QuickCheck. I use EQC here, but PropEr would also do. The deeper
%% idea is that we will probalistically make our lens correct by
%% probing it with random code.

%% The above two definitions makes for a well-behaved lens. A very
%% well behaved lens also satisfies this last rule: Last put wins and
%% behaves as if it was the only thing done to the lens.
lens_prop_putput(Gen, Val, Lens) ->
    {_Get, Put} = Lens,
    ?FORALL({X, A, A1}, {Gen, Val, Val},
            Put(A1, Put(A, X)) == Put(A1, X)).

%% EQC properties for all of these for the lens on the #r{} record. We
%% can now make sure our code works for all of these.
prop_r_a_gp() -> lens_prop_getput(gen_r(), lens_r_naive(a)).
prop_r_b_gp() -> lens_prop_getput(gen_r(), lens_r_naive(b)).
prop_r_a_pg() -> lens_prop_putget(gen_r(), int(), lens_r(a)).
prop_r_b_pg() -> lens_prop_putget(gen_r(), int(), lens_r(b)).
prop_r_a_pp() -> lens_prop_putput(gen_r(), int(), lens_r(a)).
prop_r_b_pp() -> lens_prop_putput(gen_r(), int(), lens_r(b)).

%% To make sure everything is okay, this call verifies all the
%% properties inside the module, so we make sure that we don't cheat
%% on the definitions
t() ->
    eqc:module({numtests, 1000}, ?MODULE).

