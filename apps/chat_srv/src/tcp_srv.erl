-module(tcp_srv).

-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-behavior(gen_server).

-record(state, {name,
                state,
                socket}). % the current socket

-type state() :: #state{}.

%% Exported Client Functions %% Operation & Maintenance API
-spec start_link(term()) -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link(Socket) ->
    logger:alert("start_link"),
    gen_server:start_link(?MODULE, Socket, []).

-spec stop() -> ok.

stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
-spec init(term()) -> {ok, state()}.

init(Socket) ->
    logger:alert("tcp start wait to accept"),
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket, state=connecting}}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> 'ok'.

terminate(_Reason, _State) -> ok.

-spec handle_cast(stop | accept, state()) -> {noreply, state()} | {stop, normal, state()}.

handle_cast(accept, State = #state{socket=ListenSocket}) ->
    logger:alert("tcp wait accept"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %tcp_sup:start_socket(),
    logger:alert("connecting"),
    % send response to client
    gen_tcp:send(ListenSocket, "Hey there first shell!"),
    {noreply, State#state{socket=AcceptSocket}};

handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_call(_, {_, _}, state()) -> {noreply, state()}.

handle_call(_E, _From, State) ->
    {noreply, State}.

-spec handle_info({tcp | tcp_closed | tcp_error | _, _, _}, state()) ->
    {noreply, state()} | {stop, normal, state()}.

handle_info({tcp, Socket, Str}, State) ->
    Name = <<"Name">>, % binary to iolist fun
    logger:alert("get data - ~p~n",[Str]),
    inet:setopts(Socket, [{active, once}]),
    % protocol case and disconnect case
    % disconnect user,
    % gen_tcp:close(State#state.socket),
    % {stop, normal, State};
    %gen_server:cast(self(), check_name),
    {noreply, State#state{name=Name}};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    {stop, normal, State};
handle_info(Error, State) ->
    io:format("unexpected: ~p~n", [Error]),
    {noreply, State}.



