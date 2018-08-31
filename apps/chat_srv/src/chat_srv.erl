-module (chat_srv).

-export ([check_name/1]).
-export ([login/2]).
-export ([new_user/2]).

-export([start_link/0, stop/0]).
-export([init/0, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-record(state, {users}).

-type state() :: #state{}.

%% Server API

check_name(Name) ->
    gen_server:call(?MODULE, {check_name, Name}).

login(Name, Password) ->
    gen_server:call(?MODULE, {login, Name, Password}).

new_user(Name, Password) ->
    gen_server:call(?MODULE, {new_user, Name, Password}).

%% Exported Client Functions %% Operation & Maintenance API
-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop() -> ok.

stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
-spec init() -> {ok, state()}.

init() ->
    {ok, #state{users = server:init()}}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> 'ok'.

terminate(_Reason, _State) -> ok.

-spec handle_cast(stop, state()) -> {stop, normal, state()}.

handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_call(_, {_, _}, state()) -> {noreply, state()}.

handle_call({login, Name, Password}, _From, #state{users = Users} = State) ->
    {Replay, NewUsers} = server:srv_login(Name, Password, Users),
    {replay, Replay, State#state{users = NewUsers}};

handle_call({new_user, Name, Password}, _From, #state{users = Users} = State) ->
    {Replay, NewUsers} = server:srv_new_user(Name, Password, Users),
    {replay, Replay, State#state{users = NewUsers}};

handle_call({check_name, Name}, _From, #state{users = Users} = State) ->
    Replay = server:srv_check_name(Name, Users),
    {replay, Replay, State}.


