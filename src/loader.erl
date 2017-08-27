-module(loader).

% Define native implemented functions here.
-export([load_all/0, load_all/1]).

load_all() ->
    load_all(native).

load_all(LibraryMod) -> % TODO: at some point we should use reflection to build this...
    ExportedFunctions = LibraryMod:module_info(exports),
    Functions = lists:filter(fun ({Name, _Arity}) -> Name =/= module_info end, ExportedFunctions),
    [load(LibraryMod, EF) || EF <- Functions],
    % environment:dump(0),
    ok.

% UTILS
load(LibraryMod, {Name, Arity}) ->
    FakeParams = generate_fake_params(Arity),
    Name1 = atom_to_list(Name), %Key must be a string for variable lookups to succeed!
    environment:define(Name1, {native_function, {LibraryMod, Name, FakeParams}}).

generate_fake_params(0) -> [];
generate_fake_params(Count) ->
    [fake_param(N) || N <- lists:seq(1, Count)].

% Doesn't seem like erlang can actually provide parameter names using erlang:fun_info(), so just make fake names
fake_param(Number) -> {id, "Parameter" ++ integer_to_list(Number)}.