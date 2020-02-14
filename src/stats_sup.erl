%%%-----------------------------------------------------------------------------
%%% @doc Supervisor to Start up the push_supervisor @end
%%%-----------------------------------------------------------------------------
-module(stats_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]). %% Supervisor callbacks

-define(SERVER, ?MODULE).
%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = #{strategy  => RestartStrategy,
                 intensity => MaxRestarts,
                 period    => MaxSecondsBetweenRestarts},

    PushSup = #{id       => stats_push_sup,
                start    => {stats_push_sup, start_link, []},
                restart  => permanent,
                shutdown => 5000,
                type     => supervisor,
                modules  => [stats_push_sup]},

    {ok, {SupFlags, [PushSup]}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
