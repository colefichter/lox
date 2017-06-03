#!/bin/bash

# Execute a .lox program in the lox interpreter.

# clear
#rebar3 compile
erl -noshell -pa _build/default/lib/lox/ebin/ -pa _build/test/lib/lox/test/ -s lox_app run_file $1 -s init stop