%%%-----------------------------------------------------------------------------
%%% @doc MACROS and type specific for the push functionality. @end
%%%-----------------------------------------------------------------------------
-define(PUSH_PREFIX, {stats_push, node()}).

-define(REFRESH_INTERVAL,       30000).
-define(SPIRAL_TIME_SPAN,       1000).
-define(HISTOGRAM_TIME_SPAN,    1000).
-define(STATS_LISTEN_PORT,      9000).
-define(STATS_UPDATE_INTERVAL,  1000).

-define(BUFFER,                 {buffer,    100*1024*1024}).
-define(SNDBUF,                 {sndbuf,      5*1024*1024}).
-define(ACTIVE,                 {active,    true}).
-define(REUSE,                  {reuseaddr, true}).

-define(OPTIONS,                [?BUFFER, ?SNDBUF, ?ACTIVE, ?REUSE]).

-type protocol()        :: tcp | udp | '_'.
-type instance()        :: string().
-type socket()          :: inet:socket().
-type host()            :: inet:hostname() | inet:ip4_address().
-type sanitised_push()  :: {{port(), instance(), host()}, stats:metrics()}.

-type runnning_tuple()  :: {running, boolean()}.

-type push_arg()        :: {push_key(),push_value()} | list().
-type push_key()        :: {protocol(),instance()}.
-type push_value()      :: {calendar:datetime(),
                            calendar:datetime(),
                            pid(),
                            runnning_tuple(),
                            node(),
                            port(),
                            host(),
                            stats:metrics()} | push_map() | atom().
-type push_map()        :: #{original_dt := calendar:datetime(),
                             modified_dt  := calendar:datetime(),
                             pid          := pid(),
                             running      := (true | false),
                             node         := node(),
                             port         := port(),
                             server_ip    := host(),
                             stats        := stats:metrics()}.