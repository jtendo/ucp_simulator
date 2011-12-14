all: compile

compile:
	@rebar compile

clean:
	@rebar clean

test: compile
	@rebar eunit

doc:
	rebar skip_deps=true doc

run:
	rebar clean compile
	@erl -pa deps/*/ebin -pa ebin -s lager -s ucp_simulator start
#	-pa deps/*/ebin -boot start_sasl

