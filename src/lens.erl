-module(lens).

-compile(export_all).

-record(r, {a, b}).

lens() ->
    {fun(#r { a = A}) -> A end,
     fun(R, X) -> R#r { a = X } end}.



