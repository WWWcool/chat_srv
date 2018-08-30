%%%-------------------------------------------------------------------
%% @doc tcp_sup tcp level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).
-export([empty_listeners/1]).

%% API functions
-spec start_link(list()) ->
    {ok, pid()}.

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%% Supervisor callbacks

-spec init(list()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init({Port, Max}) ->
    logger:alert("in tcp sup port - ~p and conn max - ~p", [Port, Max]),
    %% Set the socket into {active_once} mode.
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}]),
    ServerChild = {tcp_srv, {tcp_srv, start_link, [ListenSocket]},
                permanent, 2000, worker, [tcp_srv]},
    {ok, {{one_for_all, 1, 1}, [ServerChild]}}.

%init({Port, Max}) ->
%    logger:alert("in tcp sup port - ~p and conn max - ~p", [Port, Max]),
%    %% Set the socket into {active_once} mode.
%    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}]),
%    %spawn_link(?MODULE, empty_listeners, [Max]),
%    {ok,    {{simple_one_for_one, 1, 1},
%          [{tcp_srv, {tcp_srv, start_link, [ListenSocket]}, % pass the socket!
%            temporary, 1000, worker, [tcp_srv]}
%    ]}}.

start_socket() ->
    logger:alert("start tcp socket"),
    supervisor:start_child(?MODULE, []).

%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners(Max) when is_integer(Max) ->
    %logger:alert("get max - ~p", [Max]),
    [start_socket() || _ <- lists:seq(1,Max)],
    ok.



