PROJECT = beam-clients
DEPS = jsx
include erlang.mk

PATH_OPTS = -pa ebin -pa deps/jsx/ebin

run-server:
	./server/lobby_test_server.js

run-client:
	ERL $(PATH_OPTS) -s erl_client start -s erlang halt -noshell

.PHONY: run-server run-client
