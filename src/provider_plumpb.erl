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
    os:cmd("git clone git@github.com:plumlife/plum-protobufs.git protobufs"),
    ProtoFilesStr = os:cmd("find ./protobufs/lightpad/ -name \"*.proto\""),
    ProtoFiles = string:tokens(ProtoFilesStr, "\n"),
    ok = lists:foreach(fun compile_pb/1, ProtoFiles),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

compile_pb(Path) ->
    io:format("~s~n", [Path]),
    protobuffs_compile:scan_file(Path).
