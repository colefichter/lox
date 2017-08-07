-module(loader).

% Define native implemented functions here.
-export([load_all/0, load_all/1]).

load_all() ->
    load_all(native).

load_all(LibraryMod) ->
    % TODO: at some point we should use reflection to build this...
    MFA = {LibraryMod, clock, []},
    LoxCallable = {native_function, MFA},
    environment:define("clock", LoxCallable), %Key must be a string for variable lookups to succeed!
    ok.