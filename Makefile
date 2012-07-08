all:
	rebar compile

console:
	erl -pa ebin -pa deps/jsx/ebin
