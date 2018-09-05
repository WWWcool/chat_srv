%%%-------------------------------------------------------------------
%% @doc tcp_sup tcp level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_socket/0]).
-export([empty_listeners/1]).

%% Supervisor callbacks
-export([init/1]).

%% API functions
-spec start_link(list()) ->
    {ok, pid()}.

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% Supervisor callbacks

-spec init(tuple()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init({Port, Max}) ->
    %logger:alert("in tcp sup port - ~p and conn max - ~p", [Port, Max]),
    %% Set the socket into {active_once} mode.
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {reuseaddr, true}]),
    spawn_link(?MODULE, empty_listeners, [Max]),
    TCPChild = {tcp_srv, {tcp_srv, start_link, [ListenSocket]},
                transient, 2000, worker, [tcp_srv]},
    {ok,    {{simple_one_for_one, 1, 1}, [TCPChild]}}.

start_socket() ->
    %logger:alert("start tcp socket"),
    supervisor:start_child(?MODULE, []).

empty_listeners(Max) ->
    [start_socket() || _ <- lists:seq(1,Max)],
    ok.

