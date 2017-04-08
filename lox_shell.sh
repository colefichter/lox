#!/bin/bash

# Start the interactive LOX shell.

clear
rebar3 compile
erl -noshell -pa _build/default/lib/lox/ebin/ -pa _build/test/lib/lox/test/ -s lox_app repl