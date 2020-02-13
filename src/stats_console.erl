%%%-----------------------------------------------------------------------------
%%% @doc Handle console input for stats, profiles and push configuration @end
%%%-----------------------------------------------------------------------------
-module(stats_console).
-include("stats.hrl").
-include("stats_push.hrl").

-export([
    %% Stat API Exports
    show_stat/1, stat_info/1, stat_enable/1, stat_disable/1, status_change/2,
    reset_stat/1, stat_metadata/1,
        sanitise_stat_input/1, sanitise_stat_input/2, sanitise_stat_input/3,

    %% Profile API Exports
    save_profile/1, load_profile/1, load_profile_all/1, delete_profile/1,
    reset_profile/0, reset_profile_all/0,

    %% Push API Exports
    setup/1, setdown/1,
    find_push_stats/1, find_push_stats/2, find_push_stats_all/1,
        sanitise_push_input/1, store_setup_info/3,

    %% Other API
    print/1, print/2, fold_through_meta/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Stats Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%-----------------------------------------------------------------------------
%% @doc
%% Show enabled or disabled stats when using :
%%      "stats show <entry>.**"
%% enabled stats will show by default
%% @end
%%%-----------------------------------------------------------------------------
-spec(show_stat(console_arg()) -> no_return()).
show_stat(Arg) ->
    print(find_entries(sanitise_stat_input(Arg))).

%%%-----------------------------------------------------------------------------
%% @doc Returns all the stats information @end
%%%-----------------------------------------------------------------------------
-spec(stat_info(console_arg()) -> no_return()).
stat_info(Arg) ->
    {Attrs, RestArg} = pick_info_attrs(Arg),
    {Stat,Type,Status,_DPS} = sanitise_stat_input(RestArg),
    print(find_entries({Stat,Type,Status,Attrs})).

-spec(pick_info_attrs(console_arg()) -> {attributes(), console_arg()}).
%% @doc get list of attrs to print @end
pick_info_attrs(Arg) ->
    Fun = get_attr_fun(),
    case lists:foldr(Fun, {[], []}, split_arg(Arg)) of
        {[], Rest} ->          %% If no arguments given
            {?INFO_STAT, Rest}; %% use all, and return arg
        Other ->
            Other
    end.

get_attr_fun() ->
    fun
        ("name",     {As, Ps}) -> {[name      | As], Ps};
        ("type",     {As, Ps}) -> {[type      | As], Ps};
        ("module",   {As, Ps}) -> {[module    | As], Ps};
        ("value",    {As, Ps}) -> {[value     | As], Ps};
        ("cache",    {As, Ps}) -> {[cache     | As], Ps};
        ("status",   {As, Ps}) -> {[status    | As], Ps};
        ("timestamp",{As, Ps}) -> {[timestamp | As], Ps};
        ("options",  {As, Ps}) -> {[options   | As], Ps};
        (P,          {As, Ps}) -> {As, [P | Ps]}
    end.

split_arg(Str) ->
    re:split(Str, "\\-", [{return, list}]).

%%%-----------------------------------------------------------------------------
%% @doc enable the stats, if the stat is already enabled does nothing @end
%%%-----------------------------------------------------------------------------
-spec(stat_enable(console_arg()) -> no_return()).
stat_enable(Arg) -> print(status_change(Arg, enabled)).

%%%-----------------------------------------------------------------------------
%% @doc disable the stats - if already disabled does nothing @end
%%%-----------------------------------------------------------------------------
-spec(stat_disable(console_arg()) -> no_return()).
stat_disable(Arg) -> print(status_change(Arg, disabled)).

%%%-----------------------------------------------------------------------------
%% @doc
%% change the status of the stat (in metadata and) in exometer. Pulls only the
%% enabled stats if the status(es) are to be disabled, and vice versa.
%% @end
%%%-----------------------------------------------------------------------------
-spec(status_change(console_arg(), status()) -> no_return()).
status_change(Arg, ToStatus) ->
    {Entries,_DP} =
        case ToStatus of
            enabled  -> find_entries(sanitise_stat_input(Arg, '_', disabled));
            disabled -> find_entries(sanitise_stat_input(Arg, '_', enabled))
        end,
    stats_persist:change_status([{Stat, ToStatus} || {Stat,_,_} <- Entries]).

%%%-----------------------------------------------------------------------------
%% @doc resets the stats in metadata and exometer @end
%%%-----------------------------------------------------------------------------
-spec(reset_stat(console_arg()) -> ok).
reset_stat(Arg) ->
    {Found, _DPs} = find_entries(sanitise_stat_input(Arg)),
    lists:foreach(fun({N,_,_}) -> exometer:reset(N) end, Found).

%%%-----------------------------------------------------------------------------
%% @doc enabling the metadata allows the stats configuration to be persisted,
%% disabling the metadata means only using the exometer functions. @end
%%%-----------------------------------------------------------------------------
%% @doc
%% Upon re-enabling the metadata will "re-register" the stats, to remain
%% consistent between both metadata and exometer, any profile that was loaded
%% before will not be loaded upon re-enabling to prevent errors
%% @end
%%%-----------------------------------------------------------------------------
-spec(stat_metadata([string()]) -> ok).
stat_metadata(["enable" ]) -> stat_metadata(enabled);
stat_metadata(["disable"]) -> stat_metadata(disabled);
stat_metadata(["status" ]) -> stat_metadata(status);
stat_metadata(status) -> metadata_status();
stat_metadata(Arg) when is_atom(Arg) ->
    case Arg == stats_persist:enabled() of
        true  -> metadata_status();
        false -> set_metadata(Arg)
    end.

metadata_status() ->
    print_response("Metadata is ~p~n",[stats_persist:enabled()]).

set_metadata(enabled) ->
    stats_persist:reload_metadata(),
    metadata_env(enabled);
set_metadata(disabled) ->
    metadata_env(disabled).

metadata_env(Status) ->
    application:set_env(?PERSIST_APP, ?PERSIST_ENV, Status),
    metadata_status().

%%%=============================================================================
%%% Helper API
%%%=============================================================================
-define(STATUS, enabled). %% default status
-define(TYPE,   '_').     %% default type
-define(DPs,    default). %% default Datapoints
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Arguments coming in from the console arrive at this function for stats, data
%%% is transformed into a metrics name and type status/datapoints if
%%% they have been given.
%%% @end
%%%-----------------------------------------------------------------------------
-spec(sanitise_stat_input(console_arg()) -> sanitised_stat()).
sanitise_stat_input(Arg) ->
    parse_stat_entry(check_args(Arg), ?TYPE, ?STATUS, ?DPs).

%% separate Status from Type with function of same arity
sanitise_stat_input(Arg, Status)
    when   Status == enabled
    orelse Status == disabled
    orelse Status == '_' ->
    parse_stat_entry(check_args(Arg), ?TYPE, Status, ?DPs);
%% If doesn't match the clause above, it must be the Type given.
sanitise_stat_input(Arg, Type) ->
    parse_stat_entry(check_args(Arg), Type, ?STATUS, ?DPs).

sanitise_stat_input(Arg, Type, Status) ->
    parse_stat_entry(check_args(Arg), Type,  Status, ?DPs).

%%%-----------------------------------------------------------------------------
%% @doc make sure all Args are Binaries in a list: [<<Args>>]
%% sanitised for parse_stat_entry @end
%%%-----------------------------------------------------------------------------
check_args([Args]) when is_atom(Args) ->
    check_args(atom_to_binary(Args, latin1));
check_args([Args]) when is_list(Args) ->
    check_args(list_to_binary(Args));
check_args([Args]) when is_binary(Args) ->
    [Args];

check_args(Args) when is_atom(Args) ->
    check_args(atom_to_binary(Args, latin1));
check_args(Args) when is_list(Args) ->
    check_args(lists:map(fun
                             (A) when is_atom(A) ->
                                 atom_to_binary(A, latin1);
                             (A) when is_list(A) ->
                                 list_to_binary(A);
                             (A) when is_binary(A) ->
                                 A
                         end, Args));
check_args(Args) when is_binary(Args) ->
    [Args];
check_args(_) ->
    print("Illegal Argument Type ~n"), [].

parse_stat_entry(BinArgs, Type, Status, DPs) ->
    [Bin | Args] = re:split(BinArgs, "/"), %% separate /type=*.. etc...
    {NewType, NewStatus, NewDPs} =
        %% pull the type, status and datapoints from the argument given
    %% if nothing was given then the default is returned.
    type_status_and_dps(Args, Type, Status, DPs),
    StatName = statname(Bin),
    {StatName, NewStatus, NewType, NewDPs}.

%% legacy code \/
type_status_and_dps([], Type, Status, DPs) ->
    {Type, Status, DPs};
type_status_and_dps([<<"type=", T/binary>> | Rest], _T,Status,DPs) ->
    NewType =
        case T of
            <<"*">> -> '_';
            _ ->
                try binary_to_existing_atom(T, latin1)
                catch error:_ -> T
                end
        end,
    type_status_and_dps(Rest, NewType, Status, DPs);
type_status_and_dps([<<"status=", S/binary>> | Rest],Type,_St,DPs) ->
    NewStatus =
        case S of
            <<"*">> -> '_';
            <<"enabled">>  -> enabled;
            <<"disabled">> -> disabled;
            _ -> enabled
        end,
    type_status_and_dps(Rest, Type, NewStatus, DPs);
type_status_and_dps([DPsBin | Rest], Type, Status, DPs) ->
    Atoms =
        %% creates a list of the datapoints given from the command line.
    lists:foldl(fun(D,Acc) ->
        try list_to_integer(D) of
            DP -> [DP|Acc]
        catch _:_ ->
            try list_to_atom(D) of
                DP2 ->
                    [DP2|Acc]
            catch _:_ ->
                io:fwrite("Illegal datapoint name~n"),
                Acc
            end
        end
                end,[], re:split(DPsBin,",",[{return,list}])),
    NewDPs = merge(lists:flatten(Atoms),DPs),
    type_status_and_dps(Rest, Type, Status, NewDPs).

%% @doc If 'default' then
%% it will be given as 'default' to exometer anyway
merge([_ | _] = DPs, default) ->
    DPs;
%% Otherwise checks if the H from Arg is a member of the DPs list,
%% and will skip if they are already in there. @end
merge([H | T], DPs) ->
    case lists:member(H, DPs) of
        true -> merge(T, DPs);
        false -> merge(T, DPs ++ [H])
    end;
merge([], DPs) ->
    DPs.

%% @doc creates a path for the stat name @end
statname([]) ->
    [?PREFIX] ++ '_' ;
statname("*") ->
    statname([]);
statname("["++_ = Expr) ->
    case erl_scan:string(ensure_trailing_dot(Expr)) of
        {ok, Toks, _} ->
            case erl_parse:parse_exprs(Toks) of
                {ok, [Abst]} -> partial_eval(Abst);
                Error -> print("Parse error in ~p for ~p~n",
                    [Expr, Error]), []
            end;
        Error -> print("Scan Error in ~p for ~p~n",
            [Expr, Error]), []
    end;
statname(Arg) when is_binary(Arg) ->
    Parts = re:split(Arg, "\\.", [{return,list}]),
    replace_parts(Parts);
statname(_) ->
    lager:error("Illegal Argument Type~n").

ensure_trailing_dot(Str) ->
    case lists:reverse(Str) of
        "." ++ _ ->
            Str;
        _ ->
            Str ++ "."
    end.

partial_eval({cons, _, H, T}) ->
    [partial_eval(H) | partial_eval(T)];
partial_eval({tuple, _, Elems}) ->
    list_to_tuple([partial_eval(E) || E <- Elems]);
partial_eval({op, _, '++', L1, L2}) ->
    partial_eval(L1) ++ partial_eval(L2);
partial_eval(X) ->
    erl_parse:normalise(X).

replace_parts(Parts) ->
    case split(Parts, "**", []) of
        {_, []} ->
            [replace_parts_1(Parts)];
        {Before, After} ->
            Head = replace_parts_1(Before),
            Tail = replace_parts_1(After),
            [Head ++ Pad ++ Tail || Pad <- pads()]
        %% case of "**" in between elements in metric name
        %% a Pad ('_') is added in between the terms given
        %% in the arg, up to 10 times, and inserted into the
        %% list; as in:
        %% [[stat,'_',end],
        %% [stat,'_','_',end],
        %% [stat,'_','_','_',end],...];
        %% If the Argument: stat.**.end was given.
    end.

split([H | T], H, Acc) ->
    {lists:reverse(Acc), T};
split([H | T], X, Acc) ->
    split(T, X, [H | Acc]);
split([], _, Acc) ->
    {lists:reverse(Acc), []}.

replace_parts_1([H | T]) ->
    R = replace_part(H),
    case T of
        '_' -> '_';
        "**" -> [R] ++ '_';
        ["**"] -> [R] ++ '_'; %% [stat|'_']
        _ -> [R | replace_parts_1(T)]
    end;
replace_parts_1([]) ->
    [].

replace_part(H) ->
    case H of
        '_' -> '_';
        "*" -> '_';
        "'" ++ _ ->
            case erl_scan:string(H) of
                {ok, [{atom, _, A}], _} ->
                    A;
                Error ->
                    lager:error("Cannot replace part: ~p~n",
                        [Error])
            end;
        [C | _] when C >= $0, C =< $9 ->
            try list_to_integer(H)
            catch
                error:_ -> list_to_atom(H)
            end;
        _ -> list_to_atom(H)
    end.

pads() -> [lists:duplicate(N, '_') || N <- lists:seq(1,10)].

%%%-----------------------------------------------------------------------------
%% @doc
%% Find_entries for the stat show/show-0/info, each one will use find_entries to
%% print a stats information. specific for show-0 and different for info, stat
%% show is the generic base in which it was created
%% @end
%%%-----------------------------------------------------------------------------
-spec(find_entries(sanitised_stat()|console_arg()) -> found_stats()).
find_entries({Stat,Status,Type,DPs}) ->
    find_entries(Stat,Status,Type,DPs).
find_entries(Stats,Status,Type,default) ->
    find_entries(Stats,Status,Type,[]);
find_entries(Stats,Status,Type,DPs) ->
    stats:find_entries(Stats,Status,Type,DPs).


%%%-----------------------------------------------------------------------------

get_value(Name) ->
    Values = stats:get_value(Name),
    Folded = fold_values(Values),
    print_stat_args(Name, Folded).

find_stats_info({Name,Type,Status}, Info) ->
    Values = stats:get_datapoint(Name, Info),
    Folded = fold_values(Values),
    print_stat_args({Name,Type,Status,Info},Folded).

get_info_2(Statname,Attrs) ->
    fold_values([stats:get_info(Statname,Attrs)]).

print_stat_args(_StatName, []) -> [];
print_stat_args(StatName, disabled) ->
    io:fwrite("~p : disabled~n", [StatName]);
print_stat_args({StatName,Type,Status,Info},{error,_}) ->
    NewValues = get_info_2(StatName,Info),
    NewArgs = replace_type_and_status(Type,Status,NewValues),
    print_stat_args(StatName,NewArgs);
print_stat_args({Statname, Type, Status,_Info}, Args) ->
    NewArgs = replace_type_and_status(Type,Status,Args),
    print_stat_args(Statname,NewArgs);
print_stat_args(StatName, Args) ->
    io:fwrite("~p : ~p~n",[StatName,Args]).

fold_values([]) -> [];
fold_values(Values) when is_list(Values) ->
    lists:foldl(fun
                    (undefined,A)           -> A;
                    ([], A)                 -> A;
                    ({type,_},A)            -> [{type,undef}|A];
                    ({status,_},A)          -> [{status,undef}|A];
                    ({_,undefined},A)       -> A;
                    ({_,{error,_}},A)       -> A;
                    ({ms_since_reset,_V},A) -> A;
                    (DP,A)                  -> [DP|A]
                end, [], Values);
fold_values({ok,Values}) -> fold_values(Values);
fold_values(Values) -> Values.

replace_type_and_status(Type,Status,List) ->
    NewList = lists:keyreplace(type,1,List,{type,Type}),
    lists:keyreplace(status,1,NewList, {status,Status}).

print_response(String, Args) ->
    io:fwrite(String, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Profile Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @see stats_profiles.erl @doc
-spec(save_profile(stats_persist:profilename()) -> no_return()).
save_profile(ProfileName) ->
    ConcatStrName = sanitise_profile_input(ProfileName),
    print_profile("Saved",
        case profile_enabled() of
            true ->
                stats_profiles:save_profile(ConcatStrName);
            False ->
                {False, ConcatStrName}
        end).

-spec(load_profile(stats_persist:profilename()) -> no_return()).
load_profile(ProfileName) ->
    load_profile(ProfileName, [node()]).
load_profile(ProfileName,Node) ->
    ConcatStrName = sanitise_profile_input(ProfileName),
    print_profile("Loaded",
        case profile_enabled() of
            true ->
                stats_profiles:load_profile(ConcatStrName,Node);
            False ->
                {False, ConcatStrName}
        end).

-spec(load_profile_all(stats_persist:profilename()) -> no_return()).
load_profile_all(ProfileName) ->
    Nodes = [node()|nodes()],
    load_profile(ProfileName,Nodes).


-spec(delete_profile(stats_persist:profilename()) -> no_return()).
delete_profile(ProfileName) ->
    ConcatStrName = sanitise_profile_input(ProfileName),
    print_profile("Deleted",
        case profile_enabled() of
            true -> stats_profiles:delete_profile(ConcatStrName);
            False ->
                {False, ConcatStrName}
    end).

-spec(reset_profile() -> no_return()).
-spec(reset_profile([node()]) -> no_return()).
reset_profile() ->
    reset_profile([node()]).
reset_profile(Nodes) ->
    case profile_enabled() of
        true ->
            print_profile("Reset",stats_profiles:reset_profile(Nodes));
        False ->
            {False," "}
    end.

reset_profile_all() ->
    reset_profile([node()|nodes()]).

profile_enabled() ->
    case stats_persist:enabled() of
        false -> {error, persistence_disabled};
        true -> true
    end.

sanitise_profile_input(ProfileName) ->
    %% if multiple arguments are passed in from the console the name is a list
    %% of string elements -> make this all one string, not ignoring the intended
    %% spaces.
    string:join(ProfileName, " ").

print_profile(Action,{ok,ProfileName}) ->
    io:fwrite("Profile ~s : ~p ~n", [Action,ProfileName]);
print_profile(Action,{{error,Reason},ProfileName}) ->
    io:fwrite("Error, not ~s ~p -> because : ~p",[Action, ProfileName, Reason]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Push Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%-----------------------------------------------------------------------------
%%% @doc
%%% From "stats push setup ___" to this module, you can poll the stats from
%%% exometer and setup a server to push those stats to an udp endpoint.
%%% The server can be terminated to stop the pushing of stats.
%%% @end
%%%-----------------------------------------------------------------------------

-define(NEW_MAP,#{original_dt => calendar:universal_time(),
                  modified_dt => calendar:universal_time(),
                  pid => undefined,
                  running => true,
                  node => node(),
                  port => undefined,
                  server_ip => undefined,
                  stats => ['_']}).

-define(PUT_MAP(Pid,Port,Server,Stats,Map),Map#{pid => Pid,
                                                port => Port,
                                                server_ip => Server,
                                                stats => Stats}).

-define(STAT_MAP(Map), Map#{modified_dt => calendar:universal_time(),
                            running => true}).

%%%-----------------------------------------------------------------------------
%% @doc
%% The default operation of this function is to start up the pushing / polling
%% of stats from exometer to the UDP/TCP endpoint. The ability to pass in an
%% argument gives the added layer of functionality to choose the endpoint
%% details quicker and easier,
%%
%% If the server is already setup and polling stats, setting up another will
%% start up another gen_server.
%% @end
%%%-----------------------------------------------------------------------------
-spec(setup(console_arg()) -> no_return()).
setup(ListofArgs) ->
    {Protocol, Data} = sanitise_push_input(ListofArgs),
    maybe_start_server(Protocol, Data).

host_port(HostPort) ->
    [Host|Port] = re:split(HostPort,"\\:",[{return,list}]),
    {Host,Port}.

hostname([Host]) -> hostname(Host);
hostname(Host) -> list_to_atom(Host).

port([Port]) -> port(Port);
port(Port) -> list_to_integer(Port).

-spec(maybe_start_server(protocol(),sanitised_push()) -> ok | no_return()).
maybe_start_server(Protocol, {{Port, Instance,Sip},'_'}) ->
    maybe_start_server(Protocol, {{Port,Instance,Sip},['_']});
maybe_start_server(Protocol, {{Port,Instance,Sip},Stats}) ->
    case fold_through_meta(Protocol,{{'_',Instance,'_'},'_'}, [node()]) of
        [] ->
            Pid = start_server(Protocol,{{Port,Instance,Sip},Stats}),
            MapValue = ?PUT_MAP(Pid,Port,Sip,Stats,?NEW_MAP),
            store_setup_info({Protocol, Instance},MapValue,new);
        Servers ->
            maybe_start_server(Servers,Protocol,{{Port,Instance,Sip},Stats})
    end.
maybe_start_server(ServersFound,Protocol,{{Port,Instance,Sip},Stats}) ->
    lists:foreach(
        fun
            ({{_Pr,_In}, #{running := true}}) ->
                io:fwrite("Server of that instance is already running~n");
            ({{_Pr,_In}, #{running := false} = ExistingMap}) ->
                Pid =
                    start_server(Protocol, {{Port, Instance, Sip}, Stats}),
                MapValue = ?PUT_MAP(Pid,Port,Sip,Stats,ExistingMap),
                store_setup_info({Protocol, Instance}, MapValue, existing)
        end, ServersFound).

-spec(start_server(protocol(), sanitised_push()) -> no_return()).
start_server(Protocol, Arg) ->
    stats_push_sup:start_server(Protocol, Arg).

-spec(store_setup_info(push_key(),push_value(), (new | existing))
                                                     -> ok | print() | error()).
store_setup_info({_Key,_Instance},
    #{pid := NotPid}, _Type) when is_pid(NotPid) == false -> ok;
store_setup_info(Key, MapValues, new) ->
    stats_persist:put(?PUSH_PREFIX, Key, MapValues);
store_setup_info(Key, MapValues = #{running := _Bool}, existing) ->
    NewMap = ?STAT_MAP(MapValues),
    stats_persist:put(?PUSH_PREFIX, Key, NewMap).

%%%-----------------------------------------------------------------------------
%% @doc
%% Kill the udp servers currently running and pushing stats to an endpoint.
%% Stop the pushing of stats by taking the setup of the udp gen_server down
%% @end
%%%-----------------------------------------------------------------------------
-spec(setdown(console_arg()) -> no_return()).
setdown(ListofArgs) ->
    {Protocol, Data} = sanitise_push_input(ListofArgs),
    terminate_server(Protocol, Data).

%% @doc change the undefined into
-spec(terminate_server(protocol(), sanitised_push()) -> no_return()).
terminate_server(Protocol, {{Port, Instance, Sip},Stats}) ->
    stop_server(fold(Protocol, Port, Instance, Sip, Stats, node())).

stop_server(ChildrenInfo) ->
    lists:foreach(
        fun({{Protocol, Instance},#{running := true} = MapValue}) ->
            stats_push_sup:stop_server(Instance),
            stats_persist:put(?PUSH_PREFIX,
                {Protocol, Instance},
                MapValue#{modified_dt => calendar:universal_time(),
                          pid => undefined,
                          running => false})
        end, ChildrenInfo).


%%%-----------------------------------------------------------------------------
%% @doc
%% Get information on the stats polling, as in the date and time the stats
%% pushing began, and the port, server_ip, instance etc that was given at the
%% time of setup - but for all nodes in the cluster
%% @end
%%%-----------------------------------------------------------------------------
-spec(find_push_stats_all(console_arg()) -> no_return()).
find_push_stats_all(Arg) ->
    find_push_stats([node()|nodes()],Arg).

%%%-----------------------------------------------------------------------------
%% @doc
%% Get information on the stats polling, as in the date and time the stats
%% pushing began, and the port, server_ip, instance etc that was given at the
%% time of setup
%% @end
%%%-----------------------------------------------------------------------------
-spec(find_push_stats(console_arg()) -> no_return()).
find_push_stats(Arg) ->
    find_push_stats([node()], Arg).

-spec(find_push_stats([node()], console_arg()) -> print()).
find_push_stats(_Nodes,[]) ->
    print_info({error,badarg});
find_push_stats(Nodes, ["*"]) ->
    print_info(fold_through_meta('_','_','_','_','_', Nodes));
find_push_stats(Nodes,Arg) ->
    {Protocol, SanitisedData} = sanitise_push_input(Arg),
    print_info(fold_through_meta(Protocol, SanitisedData, Nodes)).

-spec(print_info(push_arg() | any()) -> ok).
print_info([]) ->
    io:fwrite("Nothing found~n");
print_info({error,badarg}) ->
    io:fwrite("Invalid Argument Given~n");
print_info({error,Reason}) ->
    io:fwrite("Error: ~p~n",[Reason]);
print_info(Info) ->
    String = "~10s ~8s ~-8s ~-15s ~-10s  ~-8s  ~-16s ~-16s ~-6s ~-8s ~s~n",
    ColumnArgs =
        ["LastDate","LastTime","Protocol","Name","Date","Time",
            "Node","ServerIp","Port","Running?","Stats"],
    io:format(String,ColumnArgs),
    lists:foreach(
        fun
            ({{Protocol,Instance},
                #{original_dt   := OriginalDateTime,
                    modified_dt := ModifiedDateTime,
                    running     := Running,
                    node        := Node,
                    port        := Port,
                    server_ip   := Sip,
                    stats       := Stats}}) ->
                {ODate,OTime} = OriginalDateTime,
                {MDate,MTime} = ModifiedDateTime,
                LastDate = consistent_date(MDate),
                LastTime = consistent_time(MTime),
                SProtocol = atom_to_list(Protocol),
                SInstance = Instance,
                Date = consistent_date(ODate),
                Time = consistent_time(OTime),
                SNode = atom_to_list(Node),
                ServerIp = Sip,
                SPort = integer_to_list(Port),
                SRunning = atom_to_list(Running),
                StatsString = io_lib:format("~p",[Stats]),
                Args = [LastDate,LastTime,SProtocol,SInstance,Date,
                    Time,SNode,ServerIp,SPort,SRunning,StatsString],
                io:format(String,Args);
            (_Other) -> ok
        end, Info).

%% the dates and time when printed have a different length, due to the
%% integer being < 10, so the lack of digits makes the list of information
%% printed out have a wonky effect.
%% This is just another layer of pretty printing.

consistent_date(Date) when is_tuple(Date) ->
    consistent_date(tuple_to_list(Date));
consistent_date(Date) when is_list(Date) ->
    NewDate = integers_to_strings(Date),
    io_lib:format("~s/~s/~s ",NewDate).

consistent_time(Time) when is_tuple(Time) ->
    consistent_time(tuple_to_list(Time));
consistent_time(Time) when is_list(Time)->
    NewTime = integers_to_strings(Time),
    io_lib:format("~s:~s:~s ",NewTime).

integers_to_strings(IntegerList) ->
    lists:map(fun(Num) when length(Num) < 2 -> "0"++Num;
                 (Num) -> Num
              end, [integer_to_list(N) || N <- IntegerList]).


-spec(fold_through_meta(protocol(),sanitised_push(),[node()]) -> [push_arg()]).
fold_through_meta(Protocol, {{Port, Instance, ServerIp}, Stats}, Nodes) ->
    fold_through_meta(Protocol,Port,Instance,ServerIp,Stats,Nodes).
fold_through_meta(Protocol, Port, Instance, ServerIp, Stats, Nodes) ->
    lists:flatten(
        [fold(Protocol, Port, Instance, ServerIp, Stats, Node)
            || Node <- Nodes]).

-spec(fold(protocol(),port(),instance(),server_ip(),metrics(),node()) ->
                                                                  [push_arg()]).
fold(Protocol, Port, Instance, ServerIp, Stats, Node) ->
    {Return, Port, ServerIp, Stats, Node} =
        cluster_metadata:fold(
            fun
                ({{MProtocol, MInstance},   %% Key would be the same
                    [#{node := MNode,
                        port := MPort,
                        server_ip := MSip,
                        stats := MStats} = MapValue]},

                    {Acc, APort, AServerIP, AStats, ANode}) %% Acc and Guard
                    when (APort     == MPort  orelse APort     == '_')
                    and  (AServerIP == MSip   orelse AServerIP == '_')
                    and  (ANode     == MNode  orelse ANode     == node())
                    and  (AStats    == MStats orelse AStats    == '_') ->
                    %% Matches all the Guards given in Acc
                    {[{{MProtocol,MInstance}, MapValue} | Acc],
                        APort, AServerIP, AStats,ANode};

                %% Doesn't Match Guards above
                ({_K, _V}, {Acc, APort, AServerIP,AStats,ANode}) ->
                    {Acc, APort, AServerIP,AStats,ANode}
            end,
            {[], Port, ServerIp, Stats, Node}, %% Accumulator
            ?PUSH_PREFIX, %% Prefix to Iterate over
            [{match, {Protocol, Instance}}] %% Key to Object match
        ),
    Return.

%%------------------------------------------------------------------------------
%% @doc
%% Sanitise the data coming in, into the necessary arguments for setting
%% up an endpoint, This is specific to the endpoint functions
%% @end
%%------------------------------------------------------------------------------
-spec(sanitise_push_input(console_arg()) -> {protocol(),sanitised_push()}).
sanitise_push_input(ListofArgs) ->
    lists:foldl(
        fun
        %% Protocol specified
            ("tcp", {_ProAcc,{{PortAcc,InAcc,HostAcc},StatAcc}}) ->
                {tcp,{{PortAcc,InAcc,HostAcc},StatAcc}};
            ("udp", {_ProAcc,{{PortAcc,InAcc,HostAcc},StatAcc}}) ->
                {udp,{{PortAcc,InAcc,HostAcc},StatAcc}};
            %% Find the Host and Port
            (HostPortOrNot, {ProAcc,{{PortAcc,InAcc,HostAcc},StatAcc}}) ->
                case host_port(HostPortOrNot) of
                    {NoHost, []} ->
                        case string:find(NoHost, ".") of
                            nomatch -> %% Then its instance
                                TheInstance = NoHost,
                                {ProAcc,{{PortAcc,TheInstance,HostAcc},StatAcc}};
                            _ -> %% Its a stat request
                                AStats = NoHost,
                                {TheStats,_,_,_} = sanitise_stat_input([AStats]),
                                {ProAcc,{{PortAcc,InAcc,HostAcc},TheStats}}
                        end;
                    {AHost,APort} ->
                        TheHost = hostname(AHost),
                        ThePort = port(APort),
                        {ProAcc,{{ThePort,InAcc,TheHost},StatAcc}}
                end
        end, {'_',{{'_','_','_'},'_'}}, ListofArgs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Other Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(print(atom()|string()|list()|{listofstats(),datapoints()}) -> print()).
print(undefined)   -> print([]);
print([undefined]) -> print([]);
print({Stats,DPs}) -> print(Stats,DPs);
print(Arg)         -> print(Arg, []).

print([], _) ->
    io:fwrite("No Matching Stats~n");
print(NewStats,Args) ->
    lists:map(fun

                  ({N,_T,_S}) when Args == [] -> get_value(N);
                  ({N, T, S}) ->  find_stats_info({N,T,S}, Args);

                  %% legacy pattern
                  (Legacy) -> legacy_map(Legacy)
              end, NewStats).

legacy_map(Legacy) ->
    lists:map(fun
                  ({LP,[]}) ->
                      io:fwrite(
                          "== ~s (Legacy pattern): No matching stats ==~n",
                          [LP]);
                  ({LP, Matches}) ->
                      io:fwrite("== ~s (Legacy pattern): ==~n",
                          [LP]),
                      [[io:fwrite("~p: ~p (~p/~p)~n",
                          [N, V, E, DP]) ||
                          {DP, V, N} <- LDPs] ||
                          {E, LDPs} <- Matches];
                  (_) ->
                      []
              end, Legacy).
