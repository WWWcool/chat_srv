-module (chat_srv).

-export ([disconnect/1]).
-export ([disconnect/2]).
-export ([check_name/1]).
-export ([login/2]).
-export ([new_user/2]).
-export ([get_rooms/0]).
-export ([join_room/2]).
-export ([quit_room/2]).
-export ([change_room/2]).
-export ([load_history/1]).
-export ([send_message/2]).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-behavior(gen_server).

-record(state, {users :: chat_server:users(), rooms :: chat_server:rooms()}).

-type state() :: #state{}.

%% Server API
-spec disconnect(iolist()) -> {ok, _}.

disconnect(Name) ->
    gen_server:call(?MODULE, {disconnect, Name}).

-spec disconnect(iolist(), term()) -> {ok, _}.

disconnect(Name, Reason) ->
    gen_server:call(?MODULE, {disconnect, Name, Reason}).

-spec check_name(iolist()) -> {ok, _}.

check_name(Name) ->
    gen_server:call(?MODULE, {check_name, Name}).

-spec login(iolist(), iolist()) -> {ok, _}.

login(Name, Password) ->
    gen_server:call(?MODULE, {login, Name, Password}).

-spec new_user(iolist(), iolist()) -> {ok, _}.

new_user(Name, Password) ->
    gen_server:call(?MODULE, {new_user, Name, Password}).

-spec get_rooms() -> {ok, _}.

get_rooms() ->
    gen_server:call(?MODULE, {get_rooms}).

-spec join_room(iolist(), iolist()) -> {ok, _}.

join_room(Name, Room) ->
    gen_server:call(?MODULE, {join_room, Name, Room}).

-spec quit_room(iolist(), iolist()) -> {ok, _}.

quit_room(Name, Room) ->
    gen_server:call(?MODULE, {quit_room, Name, Room}).

-spec change_room(iolist(), iolist()) -> {ok, _}.

change_room(Name, Room) ->
    gen_server:call(?MODULE, {change_room, Name, Room}).

-spec load_history(iolist()) -> {ok, _}.

load_history(Name) ->
    gen_server:call(?MODULE, {load_history, Name}).

-spec send_message(iolist(), iolist()) -> {ok, _}.

send_message(Name, Message) ->
    gen_server:call(?MODULE, {send_message, Name, Message}).

%% Exported Client Functions %% Operation & Maintenance API
-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.

stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
-spec init(term()) -> {ok, state()}.

init(_) ->
    {ok, #state{users = chat_server:init_users(), rooms = chat_server:init_rooms()}}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> 'ok'.

terminate(_Reason, _State) -> ok.

-spec handle_cast(stop | {resend_message, _, _}, state()) -> {stop, normal, state()}.

handle_cast({resend_message, Message, Pids}, State) ->
    % some behavior callback mb here
    lists:foreach(
        fun(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    logger:alert("Process pid - ~p", [Pid]),
                    Pid ! {resend_message, Message};
                false ->
                    logger:alert("Process not live - ~p", [Pid])
            end
        end, Pids),
    {noreply, State};

handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_call(_, {_, _}, state()) -> {reply, {ok, _},state()}.

handle_call({disconnect, Name, Reason}, From, State) ->
    logger:alert("disconnect unnormaly with reason - ~p ...", [Reason]),
    handle_call({disconnect, Name}, From, State);

handle_call({disconnect, Name}, {_From, _}, #state{users = Users} = State) ->
    logger:alert("disconnecting user - ~p ...", [Name]),
    {Reply, NewUsers} = case chat_server:get_monitor(Name, Users) of
        {ok, user_not_found} ->
            {{ok, user_not_found}, Users};
        {ok, Monitor} ->
            erlang:demonitor(Monitor),
            chat_server:disconnect(Name, Users)
    end,
    {reply, Reply, State#state{users = NewUsers}};

handle_call({check_name, Name}, _From, #state{users = Users} = State) ->
    Reply = chat_server:check_name(Name, Users),
    {reply, Reply, State};

handle_call({new_user, Name, Password}, {From, _}, #state{users = Users} = State) ->
    Connection = {From, erlang:monitor(process, From)},
    {Reply, NewUsers} = chat_server:new_user(Name, Password, Connection, Users),
    {reply, Reply, State#state{users = NewUsers}};

handle_call({login, Name, Password}, {From, _}, #state{users = Users} = State) ->
    Connection = {From, erlang:monitor(process, From)},
    {Reply, NewUsers} = chat_server:login(Name, Password, Connection, Users),
    {reply, Reply, State#state{users = NewUsers}};

handle_call({get_rooms}, _From, #state{rooms = Rooms} = State) ->
    Reply = chat_server:get_rooms(Rooms),
    {reply, Reply, State};

handle_call({join_room, Name, RoomName}, _From, #state{users = Users} = State) ->
    {Reply, NewUsers} = chat_server:join_room(Name, RoomName, Users),
    {reply, Reply, State#state{users = NewUsers}};

handle_call({quit_room, Name, RoomName}, _From, #state{users = Users} = State) ->
    {Reply, NewUsers} = chat_server:quit_room(Name, RoomName, Users),
    {reply, Reply, State#state{users = NewUsers}};

handle_call({change_room, Name, RoomName}, _From, #state{users = Users} = State) ->
    {Reply, NewUsers} = chat_server:change_room(Name, RoomName, Users),
    {reply, Reply, State#state{users = NewUsers}};

handle_call({load_history, Name}, _From, #state{users = Users, rooms = Rooms} = State) ->
    Reply = chat_server:load_history(Name, Users, Rooms),
    {reply, Reply, State};

handle_call({send_message, Name, Message}, _From, #state{users = Users, rooms = Rooms} = State) ->
    {Reply, Pids, NewUsers, NewRooms} = chat_server:send_message(Name, Message, Users, Rooms),
    gen_server:cast(self(), {resend_message, Message, Pids}),
    {reply, Reply, State#state{users = NewUsers, rooms = NewRooms}}.

-spec handle_info(_, state()) -> {noreply, state()}.

handle_info({'DOWN', _Ref, process, Pid, normal}, State) ->
    logger:alert("Proc ~p down normal", [Pid]),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, Reason}, #state{users = Users} = State) ->
    {Reply, NewUsers} = chat_server:disconnect(Pid, Users),
    logger:alert("Proc ~p down with reason - ~p, disconnect reply - ~p", [Pid, Reason, Reply]),
    {noreply, State#state{users = NewUsers}}.
