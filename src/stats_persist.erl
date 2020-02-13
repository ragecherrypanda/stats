%%%-----------------------------------------------------------------------------
%%% @doc
%%% Persistent configuration of the stats, in relation to it's use in
%%% exometer_core. Since the metrics have specific, sometimes constant options
%%% used exometer_core; this module parallels the use of these stats, but keeps
%%% each one's options to be re-used. i.e. the status of the stat, whether it is
%%% enabled or disabled, is stored in the cluster-metadata, and upon restarting
%%% the node the stats will remain enabled - disabled, instead of restarting as
%%% enabled by default.
%%% @end
%%%-----------------------------------------------------------------------------
-module(stats_persist).
-include("stats.hrl").
-include_lib("cluster_metadata/include/cluster_metadata.hrl").

-export([enabled/0, reload_metadata/0, find_entries/3,
    put/3, put/4, get/2, get/3, get_all/1, get_all_stats/0, delete/2,
    register/1, change_status/1, set_options/2, unregister/1]).

-define(STAT,                  stats).
-define(STAT_PREFIX,           {?STAT, node()}).
-define(STAT_KEY(StatName),     {?STAT_PREFIX, StatName}).
-define(STAT_MAP,               #{status  => enabled,
                                  type    => undefined,
                                  options => [],
                                  aliases => []}).

%%%=============================================================================
%%% Main API
%%%=============================================================================
%%%-----------------------------------------------------------------------------
%% @doc Check the apps env for the status of the metadata @end
%%%-----------------------------------------------------------------------------
-spec(enabled() -> true | false).
enabled() ->
    app_helper:get_env(?APP,?METADATA_ENV,true).

%%%-----------------------------------------------------------------------------
%% @doc
%% reload the cluster_metadata after it has been disabled, to match the current
%% configuration of status status' in exometer_core
%% @end
%%%-----------------------------------------------------------------------------
-spec(reload_metadata() -> ok | error()).
reload_metadata() ->
    Stats = stats_manager:find_entries([[riak]],'_'),
    change_status([{Stat,Status} || {Stat,_Type,Status} <- Stats]).

%%%-----------------------------------------------------------------------------
%% @doc
%% Use cluster_metadata:fold/4 to fold over the path in the metadata and pull
%% out the stats that match the STATUS and TYPE.
%% @end
%%%-----------------------------------------------------------------------------
-spec(find_entries(metrics(),status(),type()) -> listofstats()).
find_entries(Stats,Status,Type) ->
    %% This isn't what it looks like, it is not taking every single stat out,
    %% when the data is sanitised for "stat.**.name", it creates a list within
    %% a list such as: [[stat,'_',name],[stat,'_','_',name]...] for each one
    %% of these we need to do a fold. @see stats_console:sanitise_stat_input/1
    lists:foldl(fun(S,Acc) -> [fold_stat(S,Status,Type)|Acc] end,[],Stats).

%%%-----------------------------------------------------------------------------
%% @doc
%% Pass the StatName into the fold, to match ({match,Stat}) the objects
%% stored in the metadata, under the Prefix: @see :  ?STAT_PREFIX
%%%-----------------------------------------------------------------------------
-spec(fold_stat(metricname(),status(),(type() | '_')) -> acc()).
%%%-----------------------------------------------------------------------------
%% Returns the same as exometer:find_entries ->
%%          [{Name,Type,Status}|...]
%% @end
%%%-----------------------------------------------------------------------------
fold_stat(Stat, Status0, Type0) ->
    {Stats, _, _} =
        cluster_metadata:fold(
            fun({Name, [#{status := MStatus, type := MType}]},{Acc,Status,Type})
                when    (Status == MStatus  orelse Status == '_')
                andalso (MType  == Type     orelse Type   == '_')->
                {[{Name, MType, MStatus}|Acc],Status,Type};

                (_Other, {Acc, Status, Type}) ->
                    {Acc, Status, Type}
            end,
            {[], Status0, Type0}, ?STAT_PREFIX, [{match, Stat}]),
    Stats.

%%%-----------------------------------------------------------------------------
%% @doc Checks the metadata for the pkey provided @end
%%%-----------------------------------------------------------------------------
-spec(check_meta(metricname() | metadata_pkey()) -> metadata_value()).
check_meta(Stat) when is_list(Stat) ->
    check_meta(?STAT_KEY(Stat));
check_meta({Prefix, Key}) ->
    case get(Prefix, Key) of
        undefined -> % Not found, return empty list
            [];
        Value ->
            case find_unregister_status(Value) of
                false        -> Value;
                unregistered -> unregistered;
                Other        -> Other
            end
    end.

find_unregister_status('$deleted')                -> unregistered;
find_unregister_status(#{status := unregistered}) -> unregistered;
find_unregister_status(Map) when is_map(Map)      -> Map;
find_unregister_status(_)                         -> false.
%% if the value is anything but '$deleted' or unregistered, it returns
%% false -> i.e. it is not unregistered/deleted.

%% deleted is profile specific, unregistered is metric specific.

%%%=============================================================================
%%% Basic API
%%%=============================================================================
%%%-----------------------------------------------------------------------------
%% @doc Put into the metadata @end
%%%-----------------------------------------------------------------------------
-spec(put(metadata_prefix(), metadata_key(),
    metadata_value() | metadata_modifier(), options()) -> ok).
put(Prefix, Key, Value) ->
    put(Prefix, Key, Value, []).
put(Prefix, Key, Value, Opts) ->
    cluster_metadata:put(Prefix, Key, Value, Opts).

%%%-----------------------------------------------------------------------------
%% @doc Pulls out information from cluster_metadata @end
%%%-----------------------------------------------------------------------------
-spec(get(metadata_prefix(), metadata_key()) ->
					metadata_value() | undefined).
get(Prefix, Key) ->
    get(Prefix, Key, []).
get(Prefix, Key, Opts) ->
    cluster_metadata:get(Prefix, Key, Opts).

%%%-----------------------------------------------------------------------------
%% @doc
%% Give a Prefix for anything in the metadata and get a list of all the
%% data stored under that prefix
%% @end
%%%-----------------------------------------------------------------------------
-spec(get_all(metadata_prefix()) -> metadata_value()).
get_all(Prefix) ->
    cluster_metadata:to_list(Prefix).

get_all_stats() ->
    get_all(?STAT_PREFIX).

%%%-----------------------------------------------------------------------------
%% @doc deleting the key from the metadata replaces values with tombstone @end
%%%-----------------------------------------------------------------------------
-spec(delete(metadata_prefix(), metadata_key()) -> ok).
delete(Prefix, Key) ->
    cluster_metadata:delete(Prefix, Key).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Stats API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%-----------------------------------------------------------------------------
%% @doc
%% Checks if the stat is already registered in the metadata, if not it registers
%% it, and pulls out the options and sends it back to go into exometer_core
%% @end
%%%-----------------------------------------------------------------------------
-spec(register(tuple_stat()) -> [] | no_return()).
register({StatName, Type, Opts, Aliases}) ->
    register(StatName, Type, Opts, Aliases).
register(StatName, Type, Opts, Aliases) ->
    case check_meta(?STAT_KEY(StatName)) of
        unregistered -> [];

        [] -> re_register(StatName,{enabled,Type,Opts,Aliases}),
              Opts;

        MapValue = #{status := MStatus} ->
            {Status,NewOpts} =
                case proplists:get_value(status, Opts) of
                    undefined -> {MStatus,[{status, MStatus}|Opts]};
                    _ ->
                        Opts2=lists:keyreplace(status,1,Opts,{status,MStatus}),
                        {MStatus, Opts2}
                end,
            re_register(StatName,MapValue#{status=>Status, options => NewOpts}),
            NewOpts;

        _ -> lager:debug(
            "Could not register stat:~n{~p,[{~p,~p,~p,~p}]}~n",
            [StatName,undefined,Type,Opts,Aliases]),
            []
    end.

-spec(re_register(metricname(), (tuple_stat() | stat_map())) -> ok).
re_register(StatName,{Status,Type,Options,Aliases}) ->
    StatMap = ?STAT_MAP,
    Value = StatMap#{status => Status,
                     type => Type,
                     options => Options,
                     aliases => Aliases},
    re_register(StatName,Value);
re_register(StatName, Value) ->
    put(?STAT_PREFIX, StatName, Value).

%%%-----------------------------------------------------------------------------
%% @doc Changes the status of stats in the metadata @end
%%%-----------------------------------------------------------------------------
-spec(change_status(metricname(), status()) -> ok | acc()).
change_status({StatName, Status}) -> change_status(StatName, Status);
change_status(Stats) ->
    [change_status(Stat, Status) || {Stat, Status} <- Stats].
change_status(Statname, ToStatus) ->
    case check_meta(?STAT_KEY(Statname)) of
        []           -> []; %% doesn't exist
        unregistered -> []; %% unregistered
        MapValue ->
            put(?STAT_PREFIX,Statname,MapValue#{status=>ToStatus}),
            {Statname,ToStatus}
    end.

%%%-----------------------------------------------------------------------------
%% @doc Setting the options in the metadata @end
%%%-----------------------------------------------------------------------------
-spec(set_options(metricname() | metadata_key(), options()) -> ok).
set_options(StatInfo, NewOpts) when is_list(NewOpts) ->
    lists:foreach(fun({Key, NewVal}) ->
        set_options(StatInfo, {Key, NewVal})
                  end, NewOpts);
set_options({Statname, {Status, Type, Opts, Aliases}}, {Key, NewVal}) ->
    NewOpts = lists:keyreplace(Key, 1, Opts, {Key, NewVal}),
    set_options(Statname, {Status, Type, NewOpts, Aliases});
set_options(StatName, {Status, Type, NewOpts, Aliases}) ->
    re_register(StatName, {Status, Type, NewOpts, Aliases}).


%%%-----------------------------------------------------------------------------
%% @doc
%% Marks the stats as unregistered, that way when a node is restarted and
%% registers the stats it will be ignored
%% @end
%%%-----------------------------------------------------------------------------
-spec(unregister(metadata_key()) -> ok).
unregister(Statname) ->
    case check_meta(?STAT_KEY(Statname)) of
        MapValue = #{status := Status} when Status =/= unregistered ->
            %% Stat exists, re-register with unregistered - "status"
            put(Statname,MapValue#{status=>unregistered});
        _ -> ok
    end.