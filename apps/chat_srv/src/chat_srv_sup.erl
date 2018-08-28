%%%-------------------------------------------------------------------
%% @doc chat_srv top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type ip() :: {_, _, _, _}.

%% API functions
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link(?MODULE, []).

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    Flags = #{strategy => one_for_all},
    {ok, {Flags, [get_cowboy_child_spec({0, 0, 0, 0}, 8080)]}}.

-spec get_cowboy_child_spec(ip(), integer()) ->
    supervisor:child_spec().

get_cowboy_child_spec(IP, Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, chat_srv, "index.html"}},
            {"/websocket", web_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, chat_srv, "static"}}
        ]}
    ]),
    ranch:child_spec(
        ?SERVER,
        ranch_tcp,
        [
            {ip, IP},
            {port, Port},
            {num_acceptors, 4}
        ],
        cowboy_clear,
        #{
            env => #{dispatch => Dispatch}
        }
    ).
