-module(tcp_srv).

-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-behavior(gen_server).

-record(state, {name,
                password,
                room,
                message,
                socket}). % the current socket

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
    {ok, #state{socket=Socket}}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> 'ok'.

terminate(_Reason, _State) -> ok.

-spec handle_cast(stop | accept, state()) -> {noreply, state()} | {stop, normal, state()}.

handle_cast(accept, State = #state{socket = ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    tcp_sup:start_socket(),
    % create monitor to chat_server
    gen_tcp:send(AcceptSocket, to_binary({ok, login})),
    {noreply, State#state{socket = AcceptSocket}};

handle_cast(get_name, #state{socket = Socket, name = Name} = State) ->
    Result = chat_srv:check_name(Name),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(get_old_password, #state{socket = Socket, password = Password, name = Name} = State) ->
    Result = chat_srv:login(Name, Password),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(get_new_password, #state{socket = Socket, password = Password, name = Name} = State) ->
    Result = chat_srv:new_user(Name, Password),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(get_rooms, #state{socket = Socket} = State) ->
    Result = chat_srv:get_rooms(),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(join_room, #state{name = Name, room = Room, socket = Socket} = State) ->
    Result = chat_srv:join_room(Name, Room),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(quit_room, #state{name = Name, room = Room, socket = Socket} = State) ->
    Result = chat_srv:quit_room(Name, Room),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(change_room, #state{name = Name, room = Room, socket = Socket} = State) ->
    Result = chat_srv:change_room(Name, Room),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(load_history, #state{name = Name, socket = Socket} = State) ->
    Result = chat_srv:load_history(Name),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(send_message, #state{name = Name, message = Message, socket = Socket} = State) ->
    Result = chat_srv:send_message(Name, Message),
    Replay = to_binary(Result),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast({resend_message, Message}, #state{socket = Socket} = State) ->
    Replay = to_binary({new_message, Message}),
    gen_tcp:send(Socket, Replay),
    {noreply, State};

handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_call(_, {_, _}, state()) -> {noreply, state()}.

handle_call(_E, _From, State) ->
    {noreply, State}.

-spec handle_info({tcp | tcp_closed | tcp_error | _, _, _}, state()) ->
    {noreply, state()} | {stop, normal, state()}.

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
            {send_message, State#state{message = Message}}
    end,
    case Cast of
        disconnect ->
            gen_tcp:close(Socket),
            chat_srv:disconnect(NewState#state.name),
            {stop, normal, NewState};
        _ ->
            inet:setopts(Socket, [{active, once}]),
            gen_server:cast(self(), Cast),
            {noreply, NewState}
    end;
handle_info({resend_message, Message}, #state{socket = Socket} = State) ->
    Replay = to_binary({new_message, Message}),
    gen_tcp:send(Socket, Replay),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    chat_srv:disconnect(State#state.name),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    chat_srv:disconnect(State#state.name),
    {stop, normal, State};
handle_info(Error, State) ->
    chat_srv:disconnect(State#state.name),
    io:format("unexpected: ~p~n", [Error]),
    {noreply, State}.

to_binary(Term) ->
    erlang:term_to_binary(Term).

to_tuple(Binary) ->
    erlang:binary_to_Term(Binary).
