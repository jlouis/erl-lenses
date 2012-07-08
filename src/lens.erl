-module(lens).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%% INTRODUCTION
%% ----------------------------------------------------------------------

%% This code base example is a "Functional Lens Library" for Erlang.
%% The idea of a Lens is to provide an abstraction which is
%% * like an accessor/mutator pair in imperative OO languages, but
%% * Is functional and algebraically composable
%%
%% This file documents lenses and their use in Erlang. The hope is
%% that over time this will develop into a library for lenses in
%% Erlang, even though what I will be able to do at spawn(fun() ->
%% fest(2012) end) will be limited in extent.
%%
%% Much credit has to be given to Benjamin C. Pierce for describing
%% lenses initially and to Sebastiaan Visser and many other members
%% of the Haskell community for first writing up lenses and
%% popularizing them.

%% LENS DEFINITIONS
%% ----------------------------------------------------------------------

%% A dummy record which we use for doing the initial testing stuff on lenses.
-record(r, {a :: integer(),
            b :: integer() }).

%% A lens accessor for the 'a' parameter in the record. In due time,
%% these can be generated automatically by a parse transform
%% Likewise for the 'b' parameter.
lens_r_naive(a) -> {fun(#r { a = A}) -> A end,
                    fun(X, R) -> R#r { a = X } end};
%% Note that a Lens L = {Get, Put} consists of two parts. A "getter"
%% which can access an element and a mutator called "Put" which
%% functionally updates an element inside the lens.
lens_r_naive(b) -> {fun(#r { b = A }) -> A end,
                    fun(X, R) -> R#r { b = X } end}.

%% This will get tedious in the long run to write the above. Abstract!
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

%% We manually derive the needed lenses, here. In a fuller
%% implementation, we would use a parse transform
%% -lenses([record1, record2, ...]). To automatically derive lens
%% accessors for the records. But for now, we will limit ourselves to
%% not doing so. There are other parts which are more important to get
%% up and running.
lens_color(r) -> access_e(#color.r);
lens_color(g) -> access_e(#color.g);
lens_color(b) -> access_e(#color.b).

lens_kitten(name) -> access_e(#kitten.name);
lens_kitten(color) -> access_e(#kitten.color).

%% It turns out that a proplist is also a lens, if you use the right
%% functions for accessing the proplist:
access_p(Key) ->
    {fun(R) -> element(2, lists:keyfind(Key, 1, R)) end,
     fun(A, R) -> lists:keyreplace(Key, 1, R, {Key, A}) end}.
              
%% We manually derive the needed lenses
lens_color_p(r) -> access_p(r);
lens_color_p(g) -> access_p(g);
lens_color_p(b) -> access_p(b).

lens_kitten_p(name) -> access_p(name);
lens_kitten_p(color) -> access_p(color).
    
%% INITIAL PROPERTIES OF LENSES
%% ----------------------------------------------------------------------

%% In order to run EQC tests on this repository you need to have a way
%% to generate random 'r' records. This generator allows us to do
%% that, with integers as the contents.
gen_r() ->
    ?LET({A, B}, {int(), int()},
         #r { a = A, b = B}).

%% Likewise, let us generate colors and kittens:
gen_color() ->
    ?LET({R, G, B}, {real(), real(), real()},
         #color { r = R, g = G, b = B}).

gen_kitten() ->
    ?LET({N, C}, {binary(), gen_color()},
         #kitten { name = N, color = C}).

%% Since we also have proplist derivers for kittens:
gen_color_p() ->
    ?LET({R, G, B}, {real(), real(), real()},
         [{r, R}, {g, G}, {b, B}]).
gen_kitten_p() ->
    ?LET({N, C}, {binary(), gen_color_p()},
         [{name, N}, {color, C}]).

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
prop_r_a_gp() -> lens_prop_getput(gen_r(), lens_r(a)).
prop_r_b_gp() -> lens_prop_getput(gen_r(), lens_r(b)).
prop_r_a_pg() -> lens_prop_putget(gen_r(), int(), lens_r(a)).
prop_r_b_pg() -> lens_prop_putget(gen_r(), int(), lens_r(b)).
prop_r_a_pp() -> lens_prop_putput(gen_r(), int(), lens_r(a)).
prop_r_b_pp() -> lens_prop_putput(gen_r(), int(), lens_r(b)).

%% But the Color Proplist is also a Lens!
prop_color_p_r_gp() -> lens_prop_getput(gen_color_p(), lens_color_p(r)).
prop_color_p_g_gp() -> lens_prop_getput(gen_color_p(), lens_color_p(g)).
prop_color_p_b_gp() -> lens_prop_getput(gen_color_p(), lens_color_p(b)).
prop_color_p_r_pg() -> lens_prop_putget(gen_color_p(), real(), lens_color_p(r)).
prop_color_p_g_pg() -> lens_prop_putget(gen_color_p(), real(), lens_color_p(g)).
prop_color_p_b_pg() -> lens_prop_putget(gen_color_p(), real(), lens_color_p(b)).

%% LENS COMPOSITION
%% ----------------------------------------------------------------------

%% What sets lenses apart from the OO-style accessor/mutator pairs is
%% that they allow composition. That is, we can use lenses to form new lenses.
%%
%% Getting the composite is easy. You just "drill down" until you hit
%% whatever is there.
%% Putting requires more thought. You need to First Get out the
%% component of L so you can push that to K's put.
compose({LG, LP}, {KG, KP}) ->
    {fun(R) -> KG(LG(R)) end,
     fun(A, R) -> LP(KP(A, LG(R)), R) end}.

%% The algebra has an identity lens:
lens_id() ->
    {fun(X) -> X end,
     fun(A, _X) -> A end}.

%% To show that this forms a lens, let us run it through the EQC test
%% for the beast.
prop_id_gp() -> lens_prop_getput(int(), lens_id()).
prop_id_pg() -> lens_prop_putget(int(), int(), lens_id()).

%% Since the algebra has an identity lens, it is now trivial to build
%% a composition over a list of lenses. There is a strong indication
%% that we have a MONOID over lenses.
compose(Lenses) when is_list(Lenses) ->
    lists:foldr(fun compose/2, lens_id(), Lenses).

%% The key trick here is that we are able to compose lenses. Let's say
%% we want to manipulate the red color property of a kitten:
lens_kitten_color(r) ->
    compose([lens_kitten(color),
             lens_color(r)]).

%% Let us write a simple test for our composition...

%% Merciless ming is a kitten!
merciless_ming(black) ->
    #kitten {name = <<"merciless ming">>,
             color = #color {r = 0.0, g = 0.0, b = 0.0}};
merciless_ming(red) ->
    #kitten {name = <<"merciless ming">>,
             color = #color {r = 1.0, g = 0.0, b = 0.0}}.


simple_lens_test() ->
    Kitten = merciless_ming(black),
    {Get, Put} = lens_kitten_color(r),
    ?assertEqual(0.0, Get(Kitten)),
    ?assertEqual(merciless_ming(red), Put(1.0, Kitten)).

%% And raise it to an EQC property of the same thing
prop_kitten_color_gp() ->
    lens_prop_getput(gen_kitten(), lens_kitten_color(r)).
prop_kitten_color_pg() ->
    lens_prop_putget(gen_kitten(), real(), lens_kitten_color(r)).
     
%% That is, our algebra of lenses of lenses allows us to do ad-hoc
%% queries over deep structures by just composing existing structures
%% like in the above. And also update them in the same go via
%% modification. It is simply the function applied inside the lens.
modify({Get, Put} = _Lens, F) ->
    fun(X) ->
            Put(F(Get(X)), X)
    end.

simple_modify_test() ->
    Kitten = merciless_ming(black),
    ?assertEqual(merciless_ming(red),
                 (modify(lens_kitten_color(r), fun(C) -> C + 1.0 end))(Kitten)).


%% Missing parts:
%% * Pair-lists and lenses
%% * JSON and lenses

%% To make sure everything is okay, this call verifies all the
%% properties inside the module, so we make sure that we don't cheat
%% on the definitions
t() ->
    eunit:test({inparallel, ?MODULE}),
    eqc:module({numtests, 100}, ?MODULE).

