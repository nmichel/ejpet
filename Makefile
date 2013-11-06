build:
	./rebar compile

clean:
	./rebar clean

test: build
	./rebar eunit verbose=1 skip_deps=true
