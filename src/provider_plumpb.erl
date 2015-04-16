-module(provider_plumpb).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, plumpb).
-define(DEPS, [install_deps, compile]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},       % The 'user friendly' name of the task
            {module, ?MODULE},       % The module implementation of the task
            {bare, true},            % The task can be run by the user, always true
            {deps, ?DEPS},           % The list of dependencies
            {example, "rebar plumpb"}, % How to use the plugin
            {short_desc, "Builds Plum protobuf definitions"},
            {desc, "Checksout the protocolbuffer repository and builds Erlang modules for those definitions."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% TODO: this is yucky and unmaintainable
    os:cmd("git clone https://9cbb925e0bb80f4790d61ff7aaac5777b406f5cd@github.com/plumlife/plum-protobufs.git protobuf-defs"),
    ProtoFilesStr = os:cmd("find ./protobuf-defs/lightpad/ -name \"*.proto\""),
    ProtoFiles = string:tokens(ProtoFilesStr, "\n"),
    ok = lists:foreach(fun compile_pb/1, ProtoFiles),
    %% TODO: this is yucky and unmaintainable
    os:cmd("mv *.beam *.hrl _build/default/lib/plumpb/ebin/"),
    %% TODO: this is yucky and unmaintainable
    os:cmd("rm -rf protobuf-defs"),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

compile_pb(Path) ->
    rebar_api:console("Compiling ~s", [Path]),
    protobuffs_compile:scan_file(Path).
