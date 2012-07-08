Functional Lenses for Erlang
============================

This project implements a single (heavily commented) file, `lens.erl`
which implements functional lenses in Erlang. I've been wanting to
check out the implementation of this for some time now, and Spawnfest
2012 was as good as any occasion.

A *lens* is a generalized accessor/mutator pattern in a functional
persistent setting. It allows us to chain and compose such accessors,
much like the normal `foo.bar.baz()` chaining in most OO languages. It
also starts a library for producing ways to "dig" into structures much
like xpath queries in XML. But on the contrary it also allows for
functional update and alteration of the same structures.

The intuition of a *lens* is as a lens in a camera. When we view a
structure through the lens, we get an abstract view of what lies
behind, twisted by the lens. This allows us to manipulate the
structure behind the lens by casting it in a different light. We can
pull the structure through the lens, operate on it, and then push it
back through the lens to obtain what it originally was, but changed.

This project should be seen mostly as exploration of the concept. It
is not yet ready for drop-in into large projects. But it can over time
be made into such.

Finally, you will need eqc-mini. All the code is run through Erlang
QuickCheck in order to make sure we have a somewhat firm ground on
which to stand. That is, all our lens hacking is probalistically
checked for correctness. We also use eunit for certain simpler tests
in the code where we want to verify the point query of a given result.
If you have access to EQC mini you should be able to run `lens:t()` in
order to check the module. It runs both the eunit and EQC suites.


