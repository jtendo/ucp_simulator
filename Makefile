.PHONY: test deps

REBAR=`which rebar || ./rebar`

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

clean-deps:
	@$(REBAR) delete-deps

run: all
	erl -pa deps/*/ebin -pa ebin -s lager -s ucp_simulator start
