%%%-----------------------------------------------------------------------------
%%% @doc Server to push stats to an endpoint via UDP @end
%%%-----------------------------------------------------------------------------
-module(stats_push_udp_serv).
-include("stats.hrl").
-include("stats_push.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket        :: socket(),
                port          :: port(),
                host          :: host(),
                instance      :: instance(),
                stats         :: found_stats()}).

-define(PROTOCOL, udp).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec(start_link(Arg :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Obj) ->
    gen_server:start_link(?MODULE, Obj, []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Options]) ->
    case open(Options) of
        {ok, State} ->
            schedule_push_stats(),
            {ok, State};
        {error, Error} ->
            lager:error("Error starting ~p because: ~p", [?MODULE, Error]),
            {stop, Error}
    end.

%%------------------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%------------------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(push_stats, #state{socket       = Socket,
                               port         = Port,
                               stats        = Stats,
                               host         = Host} = State) ->
    push_stats(Socket, Host, Port, Stats),
    schedule_push_stats(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


%%------------------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(shutdown, #state{instance = Instance}) ->
    lager:info("Stopping ~p",[Instance]),
    terminate_server(Instance),
    ok;
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

open({{Port, _Instance, _Host}, _Stats}=Info) ->
    Options = ?OPTIONS,
    case gen_udp:open(Port, Options) of
        {ok, Socket} ->
            State = create_state(Socket, Info),
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

create_state(Socket, {{MonitorLatencyPort, Instance, Host}, Stats}) ->
    #state{socket        = Socket,
           port          = MonitorLatencyPort,
           host          = Host,
           instance      = Instance,
           stats         = Stats}.

%%------------------------------------------------------------------------------

terminate_server(Instance) ->
    Key = {?PROTOCOL, Instance},
    stats_push_sup:stop_running_server(?PUSH_PREFIX,Key).

%%------------------------------------------------------------------------------

send(Socket, Host, Port, Data) -> gen_udp:send(Socket, Host, Port, Data).

send_after(Interval, Arg) -> erlang:send_after(Interval,self(),Arg).

%%------------------------------------------------------------------------------
%% @doc
%% Retrieve the stats from exometer and convert to json object, to
%% send to the endpoint. Repeat.
%% @end
%%------------------------------------------------------------------------------
-spec(push_stats(socket(),host(),port(),term()) -> no_return()).
push_stats(Socket, ComponentHostname, Port, Stats) ->
    JsonStats = stats_push_util:json_stats(Stats),
    send(Socket, ComponentHostname, Port, JsonStats).

schedule_push_stats() ->
    send_after(?STATS_UPDATE_INTERVAL, push_stats).
