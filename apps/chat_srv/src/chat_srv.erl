-module (chat_srv).

-export ([disconnect/1]).
-export ([check_name/1]).
-export ([login/2]).
-export ([new_user/2]).
-export ([get_rooms/0]).
-export ([fetch_history/1]).
-export ([get_user/1]).
-export ([join_room/2]).
-export ([quit_room/2]).
-export ([change_room/2]).
-export ([load_history/1]).
-export ([send_message/2]).
-export ([receive_message/3]).

-export([start_link/0, stop/0]).
-export([init/0, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-record(state, {users, rooms}).

-type state() :: #state{}.

%% Server API
disconnect(Name) ->
    gen_server:call(?MODULE, {disconnect, Name}).

check_name(Name) ->
    gen_server:call(?MODULE, {check_name, Name}).

login(Name, Password) ->
    gen_server:call(?MODULE, {login, Name, Password}).

new_user(Name, Password) ->
    gen_server:call(?MODULE, {new_user, Name, Password}).

get_rooms() ->
    gen_server:call(?MODULE, {get_rooms}).

fetch_history(Name) ->
    gen_server:call(?MODULE, {fetch_history, Name}).

get_user(Name) ->
    gen_server:call(?MODULE, {get_user, Name}).

join_room(Name, Room) ->
    gen_server:call(?MODULE, {join_room, Name, Room}).

quit_room(Name, Room) ->
    gen_server:call(?MODULE, {quit_room, Name, Room}).

change_room(Name, Room) ->
    gen_server:call(?MODULE, {change_room, Name, Room}).

load_history(Name) ->
    gen_server:call(?MODULE, {load_history, Name}).

send_message(Name, Message) ->
    gen_server:call(?MODULE, {send_message, Name, Message}).

receive_message(Name, RoomName, Message) ->
    gen_server:call(?MODULE, {receive_message, Name, RoomName, Message}).

%% Exported Client Functions %% Operation & Maintenance API
-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop() -> ok.

stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
-spec init() -> {ok, state()}.

init() ->
    {ok, #state{users = server:srv_init_users(), rooms = server:srv_init_rooms()}}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> 'ok'.

terminate(_Reason, _State) -> ok.

-spec handle_cast(stop, state()) -> {stop, normal, state()}.

handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_call(_, {_, _}, state()) -> {noreply, state()}.

handle_call({send_message, Name, Message}, _From, #state{users = Users, rooms = Rooms} = State) ->
    {Replay, NewUsers, NewRooms} = server:srv_send_message(Name, Message, Users, Rooms),
    {replay, Replay, State#state{users = NewUsers, rooms = NewRooms}};

handle_call({load_history, Name}, _From, #state{users = Users, rooms = Rooms} = State) ->
    Replay = server:srv_load_history(Name, Users, Rooms),
    {replay, Replay, State};

handle_call({change_room, Name, RoomName}, _From, #state{users = Users} = State) ->
    {Replay, NewUsers} = server:srv_change_room(Name, RoomName, Users),
    {replay, Replay, State#state{users = NewUsers}};

handle_call({quit_room, Name, RoomName}, _From, #state{users = Users} = State) ->
    {Replay, NewUsers} = server:srv_quit_room(Name, RoomName, Users),
    {replay, Replay, State#state{users = NewUsers}};

handle_call({join_room, Name, RoomName}, _From, #state{users = Users} = State) ->
    {Replay, NewUsers} = server:srv_join_room(Name, RoomName, Users),
    {replay, Replay, State#state{users = NewUsers}};

handle_call({get_user, Name}, _From, #state{users = Users} = State) ->
    Replay = server:srv_get_user(Name, Users),
    {replay, Replay, State};

handle_call({fetch_history, Name}, _From, #state{users = Users, rooms = Rooms} = State) ->
    Replay = server:srv_fetch_history(Name, Users, Rooms),
    {replay, Replay, State};

handle_call({get_rooms}, _From, #state{rooms = Rooms} = State) ->
    Replay = server:srv_get_rooms(Rooms),
    {replay, Replay, State};

handle_call({login, Name, Password}, {_From, _}, #state{users = Users} = State) ->
    {Replay, NewUsers} = server:srv_login(Name, Password, Users),
    % monitor here
    {replay, Replay, State#state{users = NewUsers}};

handle_call({new_user, Name, Password}, {_From, _}, #state{users = Users} = State) ->
    {Replay, NewUsers} = server:srv_new_user(Name, Password, Users),
    % monitor here
    {replay, Replay, State#state{users = NewUsers}};

handle_call({check_name, Name}, _From, #state{users = Users} = State) ->
    Replay = server:srv_check_name(Name, Users),
    {replay, Replay, State};

handle_call({disconnect, Name}, {_From, _}, #state{users = Users} = State) ->
    {Replay, NewUsers} = server:srv_disconnect(Name, Users),
    % demonitor here
    {replay, Replay, State#state{users = NewUsers}}.

