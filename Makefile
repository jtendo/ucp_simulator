REBAR=`which rebar || echo ./rebar`

.PHONY: test deps

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
	erl -pa deps/*/ebin -pa ebin -boot start_sasl -s lager -s ucp_simulator_app
