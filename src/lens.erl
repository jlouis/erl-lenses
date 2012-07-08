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
%% In the longer run, it can be expanded further so we get a full
%% lens-library with parse transforms on top as well. But for now, we
%% mostly go for a simple analysis of the library - written over the
%% course of roughly 24 hours on and off. The sun is shining and
%% drinking beer in the sun also has priority in this world :)
%%
%% Much credit has to be given to Benjamin C. Pierce for describing
%% lenses initially and to Sebastiaan Visser and many other members
%% of the Haskell community for first writing up lenses and
%% popularizing them.
%%
%% Also read something like ``Combinators for Bi-Directional Tree
%% Transformations: A Linguistic Approach to the View Update Problem''
%% by J. Nathan Foster et.al. It contains a lot of information how to
%% extend these ideas to a full language of Bidirectional programming.

%% A NOTE ABOUT PERFORMANCE
%% ----------------------------------------------------------------------

%% This is not built for the sake of raw conversion speed. If you need
%% that, you need something else. This is built because it is
%% extremely flexible when you want to do surgery on different
%% structures in Erlang.
%%
%% The coolest part is that you can use a lens library to operate
%% while transforming one data structure into a another. As an
%% example, the Harmony project can convert seamlessly between
%% different bookmark formats of IE, Chrome, Firefox and so on because
%% it can abstract the structure, alter it the right way, and
%% concretize the structure for one of the other browsers.
%%
%% Getting the same ability in Erlang is the goal of this library.

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
     fun(A, R) -> lists:keystore(Key, 1, R, {Key, A}) end}.
              
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

%% ISOMORPHISMS
%% ----------------------------------------------------------------------

%% The cool thing about lens structures is that they provide us ways
%% to derive automated isomorphism translations betweens different
%% structures of data.
%%
%% Our current setup has kittens represented as records. But perhaps
%% we would like to have a proplist like structure of kittens inside.
%% This is achivable with a little more work.

%% Here is an abstract view of a color:
abs_lens_color() ->
    lists:unzip(
      [lens_color(r),
       lens_color(g),
       lens_color(b)]).

%% The same abstract view of colors as proplists:
abs_lens_color_p() ->
    lists:unzip(
      [lens_color_p(r),
       lens_color_p(g),
       lens_color_p(b)]).

%% We can now use these full abstract views to provide a way to
%% compute in the isomorphism, converting back and forth between
%% representations.
iso({GsF, PsF} = _FromLens, {GsT, PsT} = _ToLens) ->
    {fun(Source, Target) ->
             AbstractView = [F(Source) || F <- GsF],
             lists:foldl(fun({F, E}, Acc) -> F(E, Acc) end,
                         Target,
                         lists:zip(PsT, AbstractView))
     end,
     fun(Source, Target) ->
             AbstractView = [F(Source) || F <- GsT],
             lists:foldl(fun({F, E}, Acc) -> F(E, Acc) end,
                         Target,
                         lists:zip(PsF, AbstractView))
     end}.

iso_simple_test() ->
    C = #color { r = 1.0, g = 0.25, b = 0.30},
    {Fw, Bw} = iso(abs_lens_color(), abs_lens_color_p()),
    AsPropList = Fw(C, []),
    ?assertEqual(C, Bw(AsPropList, #color{})).

%% In general this isomorphism should hold for any color. There are
%% two directions to show:
iso_identity_fw(Gen, L1, L2, I1, I2) ->
    {Fw, Bw} = iso(L1, L2),
    ?FORALL(C, Gen,
            C == Bw(Fw(C, I1), I2)).
iso_identity_bw(Gen, L1, L2, I1, I2) ->
    {Fw, Bw} = iso(L1, L2),
    ?FORALL(C, Gen,
            C == Fw(Bw(C, I2), I1)).

prop_iso_identity_fw_color_p() ->
    iso_identity_fw(gen_color(), abs_lens_color(), abs_lens_color_p(), [], #color{}).
prop_iso_identity_bw_color_p() ->
    iso_identity_bw(gen_color_p(), abs_lens_color(), abs_lens_color_p(), [], #color{}).

%% A JSON library such as JSX encodes our structure as the following:
color_json(R,G,B) ->
    [{<<"r">>, R},
     {<<"g">>, G},
     {<<"b">>, B}].

access_j(K) ->
    Key = list_to_binary(atom_to_list(K)),
    {fun(R) -> element(2, lists:keyfind(Key, 1, R)) end,
     fun(A, R) ->
             lists:keystore(Key, 1, R, {Key, A})
     end}.

lens_color_j(r) -> access_j(r);
lens_color_j(g) -> access_j(g);
lens_color_j(b) -> access_j(b).

%% Establish that this works by "proof".
gen_color_j() ->
    ?LET({R, G, B}, {real(), real(), real()},
         color_json(R, G, B)).

prop_color_j_r_gp() -> lens_prop_getput(gen_color_j(), lens_color_j(r)).
prop_color_j_g_gp() -> lens_prop_getput(gen_color_j(), lens_color_j(g)).
prop_color_j_b_gp() -> lens_prop_getput(gen_color_j(), lens_color_j(b)).
prop_color_j_r_pg() -> lens_prop_putget(gen_color_j(), real(), lens_color_j(r)).
prop_color_j_g_pg() -> lens_prop_putget(gen_color_j(), real(), lens_color_j(g)).
prop_color_j_b_pg() -> lens_prop_putget(gen_color_j(), real(), lens_color_j(b)).

%% Now, do as before, generate an abstract view of these
abs_lens_color_j() ->
    lists:unzip(
      [lens_color_j(r),
       lens_color_j(g),
       lens_color_j(b)]).

%% We can do the exact same thing for our kitten:
lens_kitten_j(name) -> access_j(name);
lens_kitten_j(color) -> access_j(color).

abs_lens_kitten() ->
    lists:unzip(
      [lens_kitten(name),
       lens_kitten(color)]).

abs_lens_kitten_j() ->
    lists:unzip(
      [lens_kitten_j(name),
       lens_kitten_j(color)]).

%% With this, we can do the following isomorphism. The problem though
%% is that we are still handling the knowledge about colors inside kittens.
%%
%% Still, given a parse transformation, this could be automatic as well.
t2() ->
    {KittenFw, _KittenBw} = iso(abs_lens_kitten(), abs_lens_kitten_j()),
    {ColorFw, _ColorBw} = iso(abs_lens_color(), abs_lens_color_j()),
    Ming = #kitten { color = C } = merciless_ming(black),
    jsx:to_term(
      KittenFw(Ming#kitten {color = ColorFw(C, []) }, [])).
%%
%% ... <<"{\"name\":\"merciless ming\",\"color\":{\"r\":0.0,\"g\":0.0,\"b\":0.0}}">>
%%

%% But it turns out there another way. We can cast the kitten by
%% creating an abstract view where the kitten is a flattened list
%% structure. Then when we cast back the kitten into JSON, we can use
%% that list structure to recreate the kitten as JSON.
abs_lens_kitten_2() ->
    lists:unzip(
      lists:flatten(
        [lens_kitten(name),
         [compose(lens_kitten(color), L)
          || L <- [lens_color(C) || C <- [r,g,b]]]])).

abs_lens_kitten_j_2() ->
    lists:unzip(
      lists:flatten(
        [lens_kitten_j(name),
         [compose(lens_kitten_j(color), L)
          || L <- [lens_color_j(C) || C <- [r,g,b]]]])).

%% This construction is due to an error in the lens library as it
%% currently stands. The library can't cope with elements that "aren't
%% there". So you need to provide the skeletal structure of the Kitten
%% JSON object. In a future update of the library this problem can
%% probably be eliminated by following the exposition of functional
%% lenses more closely.
%%
%% The thing we are missing is "failing" lenses where they return
%% something of term Omega. If we had this, we could handle the case
%% by building up the structure from "nothing". But as it currently
%% stands, we can't.
json_kitten_init() ->
    [{<<"color">>, []}].

%% This allows us to Make kittens into JSON structures
t3() ->
    {KittenFw, _KittenBw} = iso(abs_lens_kitten_2(), abs_lens_kitten_j_2()),
    jsx:to_json(
      KittenFw(merciless_ming(black), json_kitten_init())).

%% We can also go the other way:
t4() ->
    {_KittenFw, KittenBw} = iso(abs_lens_kitten_2(), abs_lens_kitten_j_2()),
    KittenBw([{<<"name">>, <<"Dale Arden">>},
              {<<"color">>, [{<<"r">>, 1.0},
                             {<<"g">>, 1.0},
                             {<<"b">>, 0.75}]}],
             #kitten { color = #color{}}).

%% And let us make sure we can do it the right way, with a QuickCheck Property
prop_iso_identity_fw_kitten() ->
    iso_identity_fw(gen_kitten(),
                    abs_lens_kitten_2(),
                    abs_lens_kitten_j_2(),
                    json_kitten_init(),
                    #kitten { color = #color{} }).

gen_kitten_json() ->
    ?LET({Name, C}, {binary(), gen_color_j()},
         %% The order matters here currently. Due to
         %% the way the propery is written. So color
         %% has to come first. to create a canonical form.
         [{<<"color">>, C}, 
          {<<"name">>, Name}]).

prop_iso_identity_bw_kitten() ->
    iso_identity_bw(gen_kitten_json(),
                    abs_lens_kitten_2(),
                    abs_lens_kitten_j_2(),
                    json_kitten_init(),
                    #kitten { color = #color{}}).

%% At this point we have the necessary machine to convert back and
%% forth between different data. But there is a whole algebra of
%% lenses we have yet to touch: hoist, plunge, map, filter, wmap, ...
%% The above proves we can reasonably well operate with Lenses in
%% Erlang, but we do need some help to make them better.

%% One of the mistakes of the above is that it does not treat failure,
%% i.e., the Omega term correctly. To be a correct lens you need to be
%% able to do the "right thing" if there is no element under the lens
%% to get. This is why we currently needs "skeletons" in the above
%% code. If we add error handling of Omega correctly, this goes away.

%% To make sure everything is okay, this call verifies all the
%% properties inside the module, so we make sure that we don't cheat
%% on the definitions
t() ->
    eunit:test({inparallel, ?MODULE}),
    eqc:module({numtests, 100}, ?MODULE).

