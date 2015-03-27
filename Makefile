all: compile

compile:
	rebar compile

deps:
	rebar get-deps

run:
	erl -pa ebin -pa deps/*/ebin -config config/sys

clean:
	rebar clean

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@rebar skip_deps=true eunit

