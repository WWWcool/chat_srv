-module(web_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {name = ""    :: iolist(),
                monitor,
                state = disconnected}).

-type state() :: #state{}.

-spec init(_, _) -> {cowboy_websocket, _, _}.

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

-spec websocket_init(term()) -> {ok, state()}.

websocket_init(_) ->
    logger:alert("websocket_init ..."),
    Monitor = erlang:monitor(process, whereis(chat_srv)),
    {ok, #state{monitor = Monitor}}.

-spec websocket_handle(_, state()) -> {reply, _, state()} | {ok, state()}.

websocket_handle({text, Message}, #state{state = Cl_state} = State) ->
    Tuple = json_proto:decode(Message),
    logger:alert("Handle message - ~p", [Tuple]),
    {Reply, NewState} = case Tuple of
        {[{<<"disconnect">>,_}]} ->
            {{ok, disconnected}, State#state{state = disconnected}};
        {[{<<"enter_name">>, Name}]} ->
            Result = chat_srv:check_name(Name),
            {Result, State#state{name = Name}};
        {[{<<"enter_old_password">>, Password}]} ->
            Result = chat_srv:login(State#state.name, Password),
            SomeState = case Result of
                {ok, logged} -> State#state{state = connected};
                _ -> State
            end,
            {Result, SomeState};
        {[{<<"enter_new_password">>, Password}]} ->
            Result = chat_srv:new_user(State#state.name, Password),
            {Result, State#state{state = connected}};
        {[_, _]} when Cl_state == disconnected ->
            {{error, disconnected}, State};
        {[{<<"get_rooms">>, _}]} ->
            Result = chat_srv:get_rooms(),
            {Result, State};
        {[{<<"join_room">>, Room}]} ->
            Result = chat_srv:join_room(State#state.name, Room),
            {Result, State};
        {[{<<"quit_room">>, Room}]} ->
            Result = chat_srv:quit_room(State#state.name, Room),
            {Result, State};
        {[{<<"change_room">>, Room}]} ->
            Result = chat_srv:change_room(State#state.name, Room),
            {Result, State};
        {[{<<"load_history">>, _}]} ->
            Result = chat_srv:load_history(State#state.name),
            {Result, State};
        {[{<<"send_message">>, Message}]} ->
            Result = chat_srv:send_message(State#state.name, Message),
            {Result, State};
        Unknown ->
            logger:alert("get unknown message - ~p", [Unknown]),
            {{ok, disconnect}, State}
    end,
    {reply, {text, json_proto:encode(Reply)}, NewState};
websocket_handle(_Data, State) ->
    logger:alert("get unknown message - ~p", [_Data]),
    {ok, State}.

-spec websocket_info(_, state()) -> {reply, _, state()} | {ok, state()}.

% get message
websocket_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    logger:alert("server down with reason - ~p", [Reason]),
    {reply, {text, json_proto:encode({service_message, "Server down"})}, State};
websocket_info({resend_message, Message}, State) ->
    {reply, {text, json_proto:encode(Message)}, State};
websocket_info(Info, State) ->
    logger:alert("Get unexpected info - ~p", [Info]),
    {ok, State}.






