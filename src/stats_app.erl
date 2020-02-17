%%%-----------------------------------------------------------------------------
-module(stats_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start up all the applications for stats.
%% (persistence and push etc...)
%% @end
%%------------------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    ensure_all_started(),
    case stats_sup:start_link() of
        {ok, Pid} ->
            lager:info("stats started up"),
            {ok, Pid};
        Error ->
            lager:error("Error starting Supervisor: ~p",[Error]),
            Error
    end.

%%------------------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    exometer:stop(),
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

ensure_all_started() ->
    exometer:start(),
    case application:ensure_all_started(stats, permanent) of
        {ok, Apps} -> [lager:info("Started : ~p", [App]) || App <- Apps];
        {error, Reason} ->
            lager:error("Error starting Application : ~p", [Reason])
    end.