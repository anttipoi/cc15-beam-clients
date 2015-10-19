# JSON, UDP and latest erlang whistles

Preparing for code camp 2015.

Trying out Erlang for UDP and JSON processing.

This is the OTP version: it features

* an application, beam_clients_app
* a supervisor, beam_clients_sup
* a gen_server version of the erl_client.
* rebar3 build (erlang.mk is on its way out, I think).

For this functionality the OTP just confuses things, I think.

## Pre-requisites

Erlang env (Rel 18 or better). Brew provides ok on OSX.

Node (for the dummy server).

## Running

Run server and client in separate terminals.

    make run-server

    make run-client (compiles and runs)
    