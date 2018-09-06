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
    ExtPort = 8080,
    ExtTCPConnectionMax = 1,
    logger:alert("in srv sup port - ~p and conn max - ~p~n", [ExtPort, ExtTCPConnectionMax]),
    Flags = #{strategy => one_for_all},
    TCPChild = #{id     => tcp_sup,
                 start  => {tcp_sup, start_link, [{ExtPort + 1, ExtTCPConnectionMax}]},
                 restart    => permanent,
                 shutdown   => 2000,
                 type   => supervisor,
                 modules    => [tcp_sup, tcp_handler, tcp_srv]},
    ChatChild = #{id     => chat_srv,
                 start  => {chat_srv, start_link, []},
                 restart    => permanent,
                 shutdown   => 2000,
                 type   => worker,
                 modules    => [chat_srv, chat_server, chat_user, chat_room]},
    CowboyChild = get_cowboy_child_spec({0, 0, 0, 0}, ExtPort),
    {ok, {Flags, [ChatChild, CowboyChild, TCPChild]}}.

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
