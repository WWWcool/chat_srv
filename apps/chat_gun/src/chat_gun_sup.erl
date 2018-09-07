%%%-------------------------------------------------------------------
%% @doc chat_gun top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_gun_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([empty_clients/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    Name = "gun",
    Max = 2,
    logger:alert("start client sup name - ~p, count - ~p", [Name, Max]),
    Flags = #{strategy => simple_one_for_one},
    spawn_link(?MODULE, empty_clients, [Max]),
    ClientChild = #{id     => gun_client,
                 start  => {gun_client, start_link, [Name]},
                 restart    => transient,
                 shutdown   => 2000,
                 type   => worker,
                 modules    => [gun_client]},
    {ok, {Flags, [ClientChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec start_client(pos_integer()) -> {ok, pid()}.

start_client(Num) ->
    logger:alert("start client - ~p", [Num]),
    Ok = supervisor:start_child(?MODULE, [Num]),
    logger:alert("start child res - ~p", [Ok]),
    Ok.

-spec empty_clients(pos_integer()) -> ok.

empty_clients(Max) ->
    [{ok, _Pid1}, {ok, _Pid2}] = [start_client(Num) || Num <- lists:seq(1,Max)],
    ok.
