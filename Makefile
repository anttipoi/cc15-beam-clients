
run-server:
	./server/lobby_test_server.js

run-client: deps/jsx/README.md
	mix run -e ElixirClient.run_client

deps/jsx/README.md:
	mix deps.get

dist-clean:
	mix clean
	mix deps.clean --all

.PHONY: run-server run-client dist-clean
