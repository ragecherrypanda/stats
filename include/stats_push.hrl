%%%-----------------------------------------------------------------------------
%%% @doc MACROS and type specific for the push fnuctionality. @end
%%%-----------------------------------------------------------------------------
-define(PUSH_PREFIX, {stats_push, node()}).

-type protocol()        :: tcp | udp | '_'.
-type instance()        :: string().
-type server_ip()       :: inet:ip4_address() | any().
-type sanitised_push()  :: {{port(), instance(), server_ip()}, stats:metrics()}.

-type runnning_tuple()  :: {running, boolean()}.

-type push_arg()        :: {push_key(),push_value()} | list().
-type push_key()        :: {protocol(),instance()}.
-type push_value()      :: {calendar:datetime(),
                            calendar:datetime(),
                            pid(),
                            runnning_tuple(),
                            node(),
                            port(),
                            server_ip(),
                            stats:metrics()} | push_map() | atom().
-type push_map()        :: #{original_dt := calendar:datetime(),
                             modified_dt  := calendar:datetime(),
                             pid          := pid(),
                             running      := (true | false),
                             node         := node(),
                             port         := port(),
                             server_ip    := server_ip(),
                             stats        := stats:metrics()}.