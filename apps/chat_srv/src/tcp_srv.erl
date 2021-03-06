-module(tcp_srv).

-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-behavior(gen_server).

-record(state, {name = ""    :: iolist(),
                password = "":: iolist(),
                room = ""    :: iolist(),
                message = "" :: iolist(),
                socket, % the current socket
                monitor, state = disconnected}).

-type state() :: #state{}.

%% Exported Client Functions %% Operation & Maintenance API
-spec start_link(term()) -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

-spec stop() -> ok.

stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
-spec init(term()) -> {ok, state()}.

init(Socket) ->
    gen_server:cast(self(), accept),
    logger:alert("TCP starts..."),
    {ok, #state{socket=Socket}}.

-spec terminate(_, state()) -> true.

terminate(_Reason, #state{monitor = Monitor} = _State) ->
    logger:alert("TCP terminating..."),
    erlang:demonitor(Monitor).

-spec handle_cast(_, state()) -> {noreply, state()} | {stop, normal, state()}.

handle_cast(accept, State = #state{socket = ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    {ok, _Pid} = tcp_sup:start_socket(),
    Monitor = erlang:monitor(process, whereis(chat_srv)),
    ok = gen_tcp:send(AcceptSocket, to_binary({ok, login})),
    {noreply, State#state{socket = AcceptSocket, monitor = Monitor}};

handle_cast(get_name, #state{socket = Socket, name = Name} = State) ->
    Result = chat_srv:check_name(Name),
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast(get_old_password, #state{socket = Socket, password = Password, name = Name} = State) ->
    Result = chat_srv:login(Name, Password),
    NewState = case Result of
        {ok, logged} -> State#state{state = connected};
        _ -> State
    end,
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, NewState};

handle_cast(get_new_password, #state{socket = Socket, password = Password, name = Name} = State) ->
    Result = chat_srv:new_user(Name, Password),
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State#state{state = connected}};

handle_cast(_, #state{socket = Socket, state = Cl_state} = State) when Cl_state == disconnected ->
    Reply = to_binary({ok, not_logged_in}),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast(get_rooms, #state{socket = Socket} = State) ->
    Result = chat_srv:get_rooms(),
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast(join_room, #state{name = Name, room = Room, socket = Socket} = State) ->
    Result = chat_srv:join_room(Name, Room),
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast(quit_room, #state{name = Name, room = Room, socket = Socket} = State) ->
    Result = chat_srv:quit_room(Name, Room),
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast(change_room, #state{name = Name, room = Room, socket = Socket} = State) ->
    Result = chat_srv:change_room(Name, Room),
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast(load_history, #state{name = Name, socket = Socket} = State) ->
    Result = chat_srv:load_history(Name),
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast(send_message, #state{name = Name, message = Message, socket = Socket} = State) ->
    Result = chat_srv:send_message(Name, Message),
    Reply = to_binary(Result),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast({resend_message, Message}, #state{socket = Socket} = State) ->
    Reply = to_binary({new_message, Message}),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};

handle_cast(stop, State) ->
    logger:alert("TCP receive stop message..."),
    {stop, normal, State}.

-spec handle_call(_, {_, _}, state()) -> {noreply, state()}.

handle_call(_E, _From, State) ->
    {noreply, State}.

-spec handle_info(_, state()) ->
    {noreply, state()} | {stop, normal, state()}.

handle_info({'DOWN', _Ref, process, _Pid, Reason}, #state{socket = Socket} = State) ->
    logger:alert("server down with reason - ~p", [Reason]),
    Reply = to_binary({service_message, "Server down"}),
    ok = gen_tcp:send(Socket, Reply),
    ok = gen_tcp:close(Socket),
    {stop, normal, disconnect(State)};
handle_info({tcp, Socket, Str}, State) ->
    {Cast, NewState} = case to_tuple(Str) of
        {disconnect, _} ->
            {disconnect, State};
        {enter_name, Name} ->
            {get_name, State#state{name = Name}};
        {enter_old_password, Password} ->
            {get_old_password, State#state{password = Password}};
        {enter_new_password, Password} ->
            {get_new_password, State#state{password = Password}};
        {get_rooms, _} ->
            {get_rooms, State};
        {join_room, Room} ->
            {join_room, State#state{room = Room}};
        {load_history, _} ->
            {load_history, State};
        {quit_room, Room} ->
            {quit_room, State#state{room = Room}};
        {change_room, Room} ->
            {change_room, State#state{room = Room}};
        {send_message, Message} ->
            {send_message, State#state{message = Message}};
        Unknown ->
            logger:alert("get unknown message - ~p, disconnect...", [Unknown]),
            {disconnect, State}
    end,
    case Cast of
        disconnect ->
            ok = gen_tcp:close(Socket),
            {ok, disconnected} = chat_srv:disconnect(NewState#state.name),
            logger:alert("disconnect message..."),
            {stop, normal, disconnect(NewState)};
        _ ->
            ok = inet:setopts(Socket, [{active, once}]),
            gen_server:cast(self(), Cast),
            {noreply, NewState}
    end;
handle_info({resend_message, Message}, #state{socket = Socket} = State) ->
    Reply = to_binary({new_message, Message}),
    %logger:alert("send message to client - ~p", [Message]),
    ok = gen_tcp:send(Socket, Reply),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {ok, disconnected} = chat_srv:disconnect(State#state.name, tcp_closed),
    {stop, normal, disconnect(State)};
handle_info({tcp_error, _Socket, _}, State) ->
    {ok, disconnected} = chat_srv:disconnect(State#state.name, tcp_error),
    {stop, normal, disconnect(State)};
handle_info(Error, State) ->
    {ok, disconnected} = chat_srv:disconnect(State#state.name, Error),
    {noreply, disconnect(State)}.

-spec to_binary(term()) -> binary().

to_binary(Term) ->
    erlang:term_to_binary(Term).

-spec to_tuple(list()) -> term().

to_tuple(Binary) ->
    erlang:binary_to_term(erlang:list_to_binary(Binary)).

-spec disconnect(state()) -> state().
disconnect(State) ->
    State#state{state = disconnected}.

