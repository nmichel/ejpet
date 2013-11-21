VERBOSE = 0

build:
	./rebar compile

clean:
	./rebar clean

test: build
	./rebar eunit verbose=$(VERBOSE) skip_deps=true
