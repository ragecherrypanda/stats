%%%-----------------------------------------------------------------------------
%%% @doc APP specific @end
%%%-----------------------------------------------------------------------------

-define(APP,                  stats).
-define(PERSIST_APP,          cluster_metadata).
-define(PERSIST_ENV,          metadata_enabled).
-define(INFO_STAT,     [name,type,module,value,cache,status,timestamp,options]).
%%                      attributes for all the metrics stored in exometer

%%%-----------------------------------------------------------------------------
%%% @doc Stats @end
%%%-----------------------------------------------------------------------------

-define(PREFIX,         application:get_env(?APP, prefix, ?APP)).

-type app()             :: atom().
-type metrics()         :: [metricname()].
-type metricname()      :: [atom()] | [list()].

-export_type([metrics/0]).

-type status()          :: enabled | disabled | unregistered | '_'.
-type type()            :: exometer:type().

-type found_stats()     :: [{metricname(),type(),status()}]
                         | [{metricname(),status()}].
-type options()         :: [{atom(), any()}].
-type aliases()         :: [alias()].
-type alias()           :: {atom(),atom()}.

-type tuple_stat()      :: {metricname(),type(),options(),aliases()}
                         | {atom(),atom()}.

%%%-----------------------------------------------------------------------------
%%% @doc Console @end
%%%-----------------------------------------------------------------------------

-type console_arg()     ::  [string()].
-type sanitised_stat()  :: {metricname(),status(),type(),datapoints()}.
-type attributes()      :: [info()] | [].

% VALUES

-type stat_value()      :: exo_value() | values().
-type exo_value()       :: {ok, values()}.
-type values()          :: [value()] | [] | value().
-type value()           :: integer() | list() | atom() | binary().
-type incrvalue()       :: non_neg_integer() | integer() | float().

-type stat_info()       :: [{info(),values()}] | [].        %% [{value,0}...]
-type info()            :: name | type | module | value | cache| status
                           | timestamp | options | ref | datapoints | entry.