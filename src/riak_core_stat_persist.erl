%%%-------------------------------------------------------------------
%%% @doc
%%% riak_stat_meta is the middle-man for stats and
%%% riak_core_metadata. All information that needs to go into or out
%%% of the metadata for riak_stat will go through this module.
%%%
%%% As well as the API for the profiles used in riak_stat.
%%% @end
%%%-------------------------------------------------------------------
-module(riak_core_stat_persist).
-include("riak_stat.hrl").
-include("riak_core_metadata.hrl").

-export([
    maybe_meta/2,
    enabled/0,
    reload_metadata/0,
    find_entries/3,

    put/3, put/4,
    get/2, get/3,
    get_all/1,
    delete/2,

    register/1,
    change_status/1,
    set_options/2,
    unregister/1]).

-define(STAT,                  stats).
-define(STAT_PREFIX,           {?STAT, ?NODEID}).
-define(STATKEY(StatName),     {?STAT_PREFIX, StatName}).
-define(STATMAP,               #{status  => enabled,
                                type    => undefined,
                                options => [],
                                aliases => []}).

%%%===================================================================
%%% Main API
%%%===================================================================
%%%-------------------------------------------------------------------
%% @doc
%% Check the apps env for the status of the metadata, the persistence
%% of stat configuration can be disabled, for example: in case
%% the stats configuration doesn't need to be semi-permanent for a
%% length of time (i.e. testing)
%% @end
%%%-------------------------------------------------------------------
-type meta_arguments()       :: [] | any().
-spec(maybe_meta(function(), meta_arguments()) -> false | error() | any()).
maybe_meta(Function, Arguments) ->
    case enabled() of
        false -> false; %% it's disabled
        true  -> Function(Arguments)
    end.

enabled() ->
    app_helper:get_env(riak_core,?METADATA_ENV,true).

%%%-------------------------------------------------------------------
%% @doc
%% reload the metadata after it has been disabled, to match the
%% current configuration of status status' in exometer
%% @end
%%%-------------------------------------------------------------------
-spec(reload_metadata() -> ok | error()).
reload_metadata() ->
    Stats = riak_core_stats_mgr:exometer_find_entries([[riak]],'_'),
    change_status(
        [{Stat,Status} || {Stat,_Type,Status}<-Stats]).

%%%-------------------------------------------------------------------
%% @doc
%% Use riak_core_metadata:fold/4 to fold over the path in the
%% metadata and pull out the stats that match the STATUS and TYPE.
%% @end
%%%-------------------------------------------------------------------
-spec(find_entries(metrics(),status(),type()) -> listofstats()).
find_entries(Stats,Status,Type) ->
    lists:flatten(
        lists:map(
            fun(Stat) -> fold(Stat,Status,Type) end, Stats)).

%%%-------------------------------------------------------------------
%% @doc
%% Pass the StatName into the fold, to match ({match,Stat}) the objects
%% stored in the metadata, in the Prefix: @see :  ?STATPFX
%%
%% Guard is passed in with the Accumulator to pattern match to, in
%% order to return the stats needed.
%%%-------------------------------------------------------------------
-spec(fold(metricname(),status(),(type() | '_')) -> acc()).
%%%-------------------------------------------------------------------
%% Returns the same as exometer:find_entries ->
%%          [{Name,Type,Status}|...]
%% @end
%%%-------------------------------------------------------------------
fold(Stat, Status0, Type0) ->
    {Stats, Status0, Type0} =
        riak_core_metadata:fold(
            fun({Name, [#{status := MStatus, type := MType}]},
                {Acc, Status, Type})
                when    (Status == MStatus  orelse Status == '_')
                andalso (MType  == Type     orelse Type   == '_')->
                {[{Name, MType, MStatus}|Acc],Status,Type};

                (_Other, {Acc, Status, Type}) ->
                    {Acc, Status, Type}
            end,
            {[], Status0, Type0}, ?STAT_PREFIX, [{match, Stat}]),
    Stats.

%%%-------------------------------------------------------------------
%% @doc
%% Checks the metadata for the pkey provided
%% returns [] | Value
%% @end
%%%-------------------------------------------------------------------
-spec(check_meta(metricname() | metadata_pkey()) -> metadata_value()).
check_meta(Stat) when is_list(Stat) ->
    check_meta(?STATKEY(Stat));
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


%%%===================================================================
%%% Basic API
%%%===================================================================
%%%-------------------------------------------------------------------
%% @doc
%% Put into the metadata
%% @end
%%%-------------------------------------------------------------------
-spec(put(metadata_prefix(), metadata_key(),
    metadata_value() | metadata_modifier(), options()) -> ok).
put(Prefix, Key, Value) ->
    put(Prefix, Key, Value, []).
put(Prefix, Key, Value, Opts) ->
    riak_core_metadata:put(Prefix, Key, Value, Opts).

%%%-------------------------------------------------------------------
%% @doc
%% Pulls out information from riak_core_metadata
%% @end
%%%-------------------------------------------------------------------
-spec(get(metadata_prefix(), metadata_key()) ->
    metadata_value() | undefined).
get(Prefix, Key) ->
    get(Prefix, Key, []).
get(Prefix, Key, Opts) ->
    riak_core_metadata:get(Prefix, Key, Opts).

%%%-------------------------------------------------------------------
%% @doc
%% Give a Prefix for anything in the metadata and get a list of all the
%% data stored under that prefix
%% @end
%%%-------------------------------------------------------------------
-spec(get_all(metadata_prefix()) -> metadata_value()).
get_all(Prefix) ->
    riak_core_metadata:to_list(Prefix).

%%%-------------------------------------------------------------------
%% @doc
%% deleting the key from the metadata replaces values with tombstone
%% @end
%%%-------------------------------------------------------------------
-spec(delete(metadata_prefix(), metadata_key()) -> ok).
delete(Prefix, Key) ->
    riak_core_metadata:delete(Prefix, Key).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Stats API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%-------------------------------------------------------------------
%% @doc
%% Checks if the stat is already registered in the metadata, if not it
%% registers it, and pulls out the options for the status and sends it
%% back to go into exometer
%% @end
%%%-------------------------------------------------------------------
-spec(register(tuple_stat()) -> options()).
register({StatName, Type, Opts, Aliases}) ->
    register(StatName, Type, Opts, Aliases).
register(StatName,Type, Opts, Aliases) ->
    case check_meta(?STATKEY(StatName)) of
        [] ->
            {Status, MOpts} = find_status(fresh, Opts),
            re_register(StatName,{Status,Type,MOpts,Aliases}),
            [{status,Status}|MOpts];
        unregistered -> [];

        MapValue = #{options := MOptions,status := MStatus} ->
            {Status, NewOpts} =
                find_status(re_reg,{MOptions,MStatus,Opts}),
            re_register(StatName,MapValue#{status=>Status,
                options => NewOpts}),
            NewOpts;
        _ -> lager:debug(
            "riak_stat_meta:register(StatInfo) ->
            Could not register stat:~n{~p,[{~p,~p,~p,~p}]}~n",
            [StatName,undefined,Type,Opts,Aliases]),
            []
    end.

%%%-------------------------------------------------------------------
%% @doc
%% Find the status of a stat coming into the metadata.
%% If it is the first time registering then 'fresh' is the one hit,
%% and the status is pulled out of the Options() given, if no
%% status can be found, assume enabled - like exometer.
%%
%% Otherwise it is being re-registered and the status in the metadata
%% will take precedence as the status is persisted.
%% @end
%%%-------------------------------------------------------------------
-type find_status_type() :: fresh | re_reg.
-spec(find_status(find_status_type(), options()) ->
    {status(),options()}).
find_status(fresh, Opts) ->
    case proplists:get_value(status,Opts) of
        undefined -> {enabled, Opts};
        Status    -> {{status,Status},  Opts}
    end;
find_status(re_reg, {MetaOpts, MStatus, InOpts}) ->
    case proplists:get_value(status, InOpts) of
        undefined ->
            {MStatus,
                merge_options([{status,MStatus}|MetaOpts], InOpts)};
        _Status ->
            {MStatus,
                merge_options([{status,MStatus}|MetaOpts], InOpts)}
    end.

%%%-------------------------------------------------------------------
%% @doc
%% Combining Metadata's Options -> Replacing the current tuples in the
%% incoming options with the new tuples in the metadata, all the other
%% options that are not stored in the metadata are ignored
%%
%% The main options stored in the metadata are the same options that
%% are given on . @see : the_alpha_stat
%% @end
%%%-------------------------------------------------------------------
merge_options(MetadataOptions, IncomingOptions) ->
    SortedMeta = lists:ukeysort(1,MetadataOptions),
    SortedOpts = lists:ukeysort(1,IncomingOptions),
    lists:ukeymerge(1,SortedMeta,SortedOpts).

re_register(StatName,{Status,Type,Options,Aliases}) ->
    StatMap = ?STATMAP,
    Value = StatMap#{
        status => Status,
        type => Type,
        options => Options,
        aliases => Aliases},
    re_register(StatName,Value);
re_register(StatName, Value) -> %% ok
    put(?STAT_PREFIX, StatName, Value).

%%%-------------------------------------------------------------------
%% @doc
%% Changes the status of stats in the metadata
%% @end
%%%-------------------------------------------------------------------
-spec(change_status(metricname(), status()) -> ok | acc()).
change_status(Stats) when is_list(Stats) ->
    [change_status(Stat,Status)||{Stat,Status} <- Stats];
change_status({StatName, Status}) ->
    change_status(StatName, Status).
change_status(Statname, ToStatus) ->
    case check_meta(?STATKEY(Statname)) of
        []           -> []; %% doesn't exist
        unregistered -> []; %% unregistered
        MapValue ->
            put(?STAT_PREFIX,Statname,MapValue#{status=>ToStatus}),
            {Statname,ToStatus}
    end.

%%%-------------------------------------------------------------------
%% @doc
%% Setting the options in the metadata manually, such as
%% resets etc...
%% @end
%%%-------------------------------------------------------------------
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


%%%-------------------------------------------------------------------
%% @doc
%% Marks the stats as unregistered, that way when a node is restarted
%% and registers the stats it will be ignored
%% @end
%%%-------------------------------------------------------------------
-spec(unregister(metadata_key()) -> ok).
unregister(Statname) ->
    case check_meta(?STATKEY(Statname)) of
        MapValue = #{status := Status} when Status =/= unregistered ->
            %% Stat exists, re-register with unregistered - "status"
            put(Statname,MapValue#{status=>unregistered});
        _ -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Profile API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%%%===================================================================
%%% Internal API
%%%===================================================================

print(String) ->
    print(String, []).
print(String,Args) -> riak_core_console:print(String,Args).
