-module(gun_client).

-export([disconnect/1]).
-export([connect/3]).
-export([signin/3]).
-export([get_rooms/1]).
-export([join_room/2]).
-export([quit_room/2]).
-export([change_room/2]).
-export([load_history/1]).
-export([send_message/2]).

-export([start_link/2, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-behavior(gen_server).

-record(state, {server_name, connpid, pid, monitor, name, password,
                rooms = [], msg_buffer = [], state = disconnected}).

-type state() :: #state{}.

%% Client API
-spec disconnect(atom()) -> {ok, disconnected}.

disconnect(Srv) ->
    gen_server:call(Srv, {disconnect}).

-spec connect(_, pos_integer(), atom()) -> {ok, connecting}.

connect(Ip, Port, Srv) ->
    gen_server:call(Srv, {connect, Ip, Port}).

-spec signin(iolist(), iolist(), atom()) -> {ok, checking_name}.
signin(Name, Password, Srv) ->
    gen_server:call(Srv, {signin, Name, Password}).

-spec get_rooms(atom()) -> {ok, getting}.

get_rooms(Srv) ->
    gen_server:call(Srv, {get_rooms}).

-spec join_room(iolist(), atom()) -> {ok, joining} | {error, room_not_exist}.

join_room(Room, Srv) ->
    gen_server:call(Srv, {join_room, Room}).

-spec quit_room(iolist(), atom()) -> {ok, quiting} | {error, room_not_exist}.

quit_room(Room, Srv) ->
    gen_server:call(Srv, {quit_room, Room}).

-spec change_room(iolist(), atom()) -> {ok, changing} | {error, room_not_exist}.

change_room(Room, Srv) ->
    gen_server:call(Srv, {change_room, Room}).

-spec load_history(atom()) -> {ok, loading}.

load_history(Srv) ->
    gen_server:call(Srv, {load_history}).

-spec send_message(iolist(), atom()) -> {ok, sending}.

send_message(Message, Srv) ->
    gen_server:call(Srv, {send_message, Message}).

%% Exported Client Functions %% Operation & Maintenance API
-spec start_link(iolist(), pos_integer()) -> ignore | {error, _} | {ok, pid()}.

start_link(Name, Num) ->
    ServerName = erlang:list_to_atom("client_" ++ Name ++ erlang:integer_to_list(Num)),
    logger:alert("Start client - ~p", [ServerName]),
    gen_server:start_link({local, ServerName}, ?MODULE, ServerName, []).

-spec start_link(iolist()) -> ignore | {error, _} | {ok, pid()}.
start_link(Name) ->
    ServerName = erlang:list_to_atom("client_" ++ Name),
    logger:alert("Start client - ~p", [ServerName]),
    gen_server:start_link({local, ServerName}, ?MODULE, ServerName, []).

-spec stop() -> ok.

stop() -> gen_server:cast(self(), stop).

%% Callback Functions
-spec init(iolist()) -> {ok, state()}.

init(Name) ->
    logger:alert("Init client - ~p", [Name]),
    {ok, #state{server_name = Name}}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> true.

terminate(_Reason, #state{monitor = Monitor}) when is_reference(Monitor) ->
    erlang:demonitor(Monitor);
terminate(_Reason, _State) ->
    ok.

-spec handle_cast(stop, state()) -> {stop, normal, state()}.

handle_cast(signup, #state{connpid = ConnPid, password = Password} = State) ->
    ok = ws_send(ConnPid, {enter_new_password, Password}),
    {noreply, State};

handle_cast(signin, #state{connpid = ConnPid, password = Password} = State) ->
    ok = ws_send(ConnPid, {enter_old_password, Password}),
    {noreply, State};

handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_call(_, {_, _}, state()) -> {reply, {ok | error, _},state()}.

handle_call({disconnect}, _From, #state{connpid = ConnPid} = State) ->
    ok = ws_send(ConnPid, {disconnect, "dummy"}),
    ws_disconnect(ConnPid),
    {reply, {ok, disconnected}, State#state{state = disconnected}};

handle_call({connect, Ip, Port}, {From, _}, State) ->
    {ok, ConnPid} = gun:open(Ip, Port),
    logger:alert("gun opened with result connpid - ~p", [ConnPid]),
    {ok, _Protocol} = gun:await_up(ConnPid),
    logger:alert("gun awaited up - ~p", [_Protocol]),
    StreamRef = gun:ws_upgrade(ConnPid, "/websocket", [], #{compress => true}),
    logger:alert("gun upgraded StreamRef - ~p", [StreamRef]),
    Monitor = erlang:monitor(process, From),
    {reply, {ok, connecting}, State#state{pid = From, monitor = Monitor}};

handle_call({signin, Name, Password}, _From,#state{connpid = ConnPid} = State) ->
    ok = ws_send(ConnPid, {enter_name, Name}),
    {reply, {ok, checking_name}, State#state{name = Name, password = Password}};

handle_call({get_rooms}, _From, #state{connpid = ConnPid} = State) ->
    ok = ws_send(ConnPid, {get_rooms, "dummy"}),
    {reply, {ok, getting}, State#state{state = get_rooms}};

handle_call({join_room, Room}, _From, #state{connpid = ConnPid, rooms = Rooms} = State) ->
    case lists:member(Room, Rooms) of
        true ->
            ok = ws_send(ConnPid, {join_room, Room}),
            {reply, {ok, joining}, State};
        false ->
            {reply, {error, room_not_exist}, State}
    end;

handle_call({quit_room, Room}, _From, #state{connpid = ConnPid, rooms = Rooms} = State) ->
    case lists:member(Room, Rooms) of
        true ->
            ok = ws_send(ConnPid, {quit_room, Room}),
            {reply, {ok, quiting}, State};
        false ->
            {reply, {error, room_not_exist}, State}
    end;

handle_call({change_room, Room}, _From, #state{connpid = ConnPid, rooms = Rooms} = State) ->
    case lists:member(Room, Rooms) of
        true ->
            ok = ws_send(ConnPid, {change_room, Room}),
            {reply, {ok, changing}, State};
        false ->
            {reply, {error, room_not_exist}, State}
    end;

handle_call({load_history}, _From, #state{connpid = ConnPid} = State) ->
    ok = ws_send(ConnPid, {load_history, "dummy"}),
    {reply, {ok, loading}, State#state{state = load_history}};

handle_call({send_message, Message}, _From, #state{connpid = ConnPid} = State) ->
    ok = ws_send(ConnPid, {send_message, Message}),
    {reply, {ok, sending}, State}.

-spec handle_info(_, state()) -> {noreply, state()} | {stop, normal, state()}.

handle_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    logger:alert("client down with reason - ~p", [Reason]),
    {stop, normal, State};
handle_info({gun_ws, _, _, {text, Message}}, #state{connpid = ConnPid, state = Cl_state, pid = Pid} = State) when Cl_state /= disconnected ->
    logger:alert("pid - ~p get answer from server - ~p", [self(), json_proto:decode(Message)]),
    Map = json_proto:decode(Message),
    Type = maps:get(<<"type">>, Map),
    Data = maps:get(<<"data">>, Map),
    {Cast, NewState} = case {Type, Data} of
        {<<"disconnect">>, _} ->
            {disconnect, State};
        {<<"ok">>, <<"login">>} ->
            {ok, connected} = answer(Pid, {ok, connected}),
            {ok, State#state{state = connected}};
        {<<"ok">>, <<"user_not_found">>} when Cl_state == established ->
            {signup, State};
        {<<"ok">>, <<"found">>} ->
            {signin, State};
        {<<"ok">>, <<"added_and_logged">>} ->
            {ok, logged} = answer(Pid, {ok, logged}),
            {ok, State#state{state = logged}};
        {<<"ok">>, <<"bad_password">>} ->
            {error, bad_password} = answer(Pid, {error, bad_password}),
            {ok, State};
        {<<"ok">>, <<"logged">>} ->
            {ok, logged} = answer(Pid, {ok, logged}),
            {ok, State#state{state = logged}};
        {<<"ok">>, List} when Cl_state == get_rooms ->
            {rooms, List} = answer(Pid, {rooms, List}),
            {ok, State#state{state = connected, rooms = List}};
        {<<"ok">>, List} when Cl_state == load_history ->
            {history, List} = answer(Pid, {history, List}),
            {ok, State#state{state = connected, msg_buffer = List}};
        {<<"ok">>, <<"joined">>} ->
            {ok, joined} = answer(Pid, {ok, joined}),
            {ok, State};
        {<<"ok">>, <<"quited">>} ->
            {ok, quited} = answer(Pid, {ok, quited}),
            {ok, State};
        {<<"ok">>, <<"changed">>} ->
            {ok, changed} = answer(Pid, {ok, changed}),
            {ok, State};
        {<<"ok">>, <<"start_resend">>} ->
            {ok, sended} = answer(Pid, {ok, sended}),
            {ok, State};
        {<<"new_message">>, Text} ->
            {new_message, Text} = answer(Pid, {new_message, Text}),
            {ok, State};
        Unknown ->
            logger:alert("get unknown message - ~p disconnect...", [Unknown]),
            {disconnect, State}
    end,
    case Cast of
        disconnect ->
            ws_disconnect(ConnPid),
            {stop, normal, NewState#state{state = disconnected}};
        ok ->
            {noreply, NewState};
        _ ->
            gen_server:cast(self(), Cast),
            {noreply, NewState}
    end;
handle_info(Message, #state{state = Cl_state} = State) when Cl_state == disconnected ->
    logger:alert("get message befor we start - ~p", [Message]),
    NewState = case Message of
    {gun_upgrade, ConnPid, _, _, _Headers} ->
        {ok, connected} = answer(State#state.pid, {ok, connected}),
        State#state{connpid = ConnPid, state = established};
    {gun_ws_upgrade, ConnPid, ok, _Headers} ->
        {ok, connected} = answer(State#state.pid, {ok, connected}),
        State#state{connpid = ConnPid, state = established};
    {gun_response, _ConnPid, _, _, Status, _Headers} ->
        logger:alert("upgrade failed with status - ~p", [Status]),
        State;
    {gun_error, _ConnPid, _StreamRef, Reason} ->
        logger:alert("gun error with reason -~p", [Reason]),
        State
    end,
    {noreply, NewState};
handle_info(_Error, State) ->
    logger:alert("get unknown message - ~p ...", [_Error]),
    {noreply, State}.

-spec answer(pid(), tuple()) -> tuple().

answer(Pid, Message) ->
    Pid ! Message.

-spec ws_send(_, tuple()) -> ok.

ws_send(ConnPid, Tuple) ->
    Json = json_proto:encode(Tuple),
    logger:alert("send packet - ~p json - ~p", [Tuple, Json]),
    gun:ws_send(ConnPid, {text, Json}).

-spec ws_disconnect(_) -> ok.

ws_disconnect(ConnPid) ->
    gun:close(ConnPid).
