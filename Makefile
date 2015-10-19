PROJECT = beam-clients

PATH_OPTS = -pa ebin -pa deps/jsx/ebin

run-server:
	./server/lobby_test_server.js

run-client: deps/jsx
	./rebar3 compile
	erl -noshell \
	-pa _build/default/lib/beam_clients/ebin \
	-pa _build/default/lib/jsx/ebin \
	-eval "application:start(beam_clients)"

deps/jsx:
	./rebar3 update

.PHONY: run-server run-client
