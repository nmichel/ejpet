VERBOSE = 0

build:
	./rebar3 compile

clean:
	./rebar3 clean

test: build
	./rebar3 eunit verbose=$(VERBOSE) skip_deps=true

shell:
	./rebar3 as dev get-deps
	./rebar3 as dev tree
	./rebar3 as dev compile
	erl -pz ./_build/dev/lib/*/ebin
