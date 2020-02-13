
-type socket()          :: inet:socket().
-type hostname()        :: inet:hostname().

-type listofpush()      :: [pusharg()] | [].





-type jsonstats()       :: list().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Endpoint Polling Macros %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(PUSH_PREFIX, {riak_stat_push, ?NODEID}).

-define(REFRESH_INTERVAL,       app_helper:get_env(riak_stat,refresh_interval,     30000)).

-define(SPIRAL_TIME_SPAN,       app_helper:get_env(riak_stat,spiral_time_span,      1000)).
-define(HISTOGRAM_TIME_SPAN,    app_helper:get_env(riak_stat,historgram_time_span,  1000)).
-define(STATS_LISTEN_PORT,      app_helper:get_env(riak_stat,stat_listen_port,      9000)).
-define(STATS_UPDATE_INTERVAL,  app_helper:get_env(riak_stat,stat_update_interval,  1000)).

-define(BUFFER,                 {buffer,    100*1024*1024}).
-define(SNDBUF,                 {sndbuf,      5*1024*1024}).
-define(ACTIVE,                 {active,    true}).
-define(REUSE,                  {reuseaddr, true}).

-define(OPTIONS,                [?BUFFER, ?SNDBUF, ?ACTIVE, ?REUSE]).

