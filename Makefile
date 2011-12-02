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
	@erl -pa ebin -s ucp_simulator start

