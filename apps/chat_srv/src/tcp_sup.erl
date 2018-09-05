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
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {reuseaddr, true}]),
    spawn_link(?MODULE, empty_listeners, [Max]),
    Flags = #{strategy => simple_one_for_one},
    TCPChild = #{id     => tcp_srv,
                 start  => {tcp_srv, start_link, [ListenSocket]},
                 restart    => transient,
                 shutdown   => 2000,
                 type   => worker,
                 modules    => [tcp_srv]},
    {ok, {Flags, [TCPChild]}}.

start_socket() ->
    %logger:alert("start tcp socket"),
    supervisor:start_child(?MODULE, []).

empty_listeners(Max) ->
    [{ok, _Pid}] = [start_socket() || _ <- lists:seq(1,Max)],
    ok.

