%%%-----------------------------------------------------------------------------
%%% @doc
%%% Polling of stats from an endpoint and pushed to an endpoint of arguments
%%% given.
%%% @end
%%%-----------------------------------------------------------------------------
-module(stats_push).
-include("stats.hrl").
-include("stats_push.hrl").

%% API
-export([maybe_start_server/2, terminate_server/2, store_setup_info/3,
         fold_through_meta/3, find_push_stats/2]).

-define(NEW_MAP,#{original_dt => calendar:universal_time(),
                  modified_dt => calendar:universal_time(),
                  pid => undefined,
                  running => true,
                  node => node(),
                  port => undefined,
                  host => undefined,
                  stats => ['_']}).

-define(PUT_MAP(Pid,Port,Server,Stats,Map),Map#{pid   => Pid,
                                                port  => Port,
                                                host  => Server,
                                                stats => Stats}).

-define(STAT_MAP(Map), Map#{modified_dt => calendar:universal_time(),
                            running => true}).

%%%=============================================================================
%%% API
%%%=============================================================================
%%%-----------------------------------------------------------------------------
%% @doc
%% The default operation of this function is to start up the pushing / polling
%% of stats from exometer to the UDP/TCP endpoint. The ability to pass in an
%% argument gives the added layer of functionality to choose the endpoint
%% details quicker and easier.
%% @end
%%%-----------------------------------------------------------------------------
-spec(maybe_start_server(protocol(),sanitised_push()) -> ok | no_return()).
maybe_start_server(Protocol, {{Port, Instance,Host},'_'}) ->
    maybe_start_server(Protocol, {{Port,Instance,Host},['_']});
maybe_start_server(Protocol, {{Port,Instance,Host},Stats}) ->
    case fold_through_meta(Protocol,{{'_',Instance,'_'},'_'}, [node()]) of
        [] ->
            Pid = start_server(Protocol,{{Port,Instance,Host},Stats}),
            MapValue = ?PUT_MAP(Pid,Port,Host,Stats,?NEW_MAP),
            store_setup_info({Protocol, Instance},MapValue,new);
        Servers ->
            maybe_start_server(Servers,Protocol,{{Port,Instance,Host},Stats})
    end.
maybe_start_server(ServersFound,Protocol,{{Port,Instance,Host},Stats}) ->
    lists:foreach(
        fun
            ({{_Pr,_In}, #{running := true}}) ->
                io:fwrite("Server of that instance is already running~n");
            ({{_Pr,_In}, #{running := false} = ExistingMap}) ->
                Pid =
                    start_server(Protocol, {{Port, Instance, Host}, Stats}),
                MapValue = ?PUT_MAP(Pid,Port,Host,Stats,ExistingMap),
                store_setup_info({Protocol, Instance}, MapValue, existing)
        end, ServersFound).

-spec(start_server(protocol(), sanitised_push()) -> no_return()).
start_server(Protocol, Arg) ->
    stats_push_sup:start_server(Protocol, Arg).

-spec(store_setup_info(push_key(),push_value(), (new | existing)) -> ok).
store_setup_info({_Key,_Instance},
    #{pid := NotPid}, _Type) when is_pid(NotPid) == false -> ok;
store_setup_info(Key, MapValues, new) ->
    stats_persist:put(?PUSH_PREFIX, Key, MapValues);
store_setup_info(Key, MapValues = #{running := _Bool}, existing) ->
    NewMap = ?STAT_MAP(MapValues),
    stats_persist:put(?PUSH_PREFIX, Key, NewMap).

%%%-----------------------------------------------------------------------------
%% @doc
%% Kill the servers currently running and pushing stats to an endpoint.
%% Stop the pushing of stats by killing the gen_server pushing stats
%% @end
%%%-----------------------------------------------------------------------------
-spec(terminate_server(protocol(), sanitised_push()) -> no_return()).
terminate_server(Protocol, {{Port, Instance, Host},Stats}) ->
    stop_server(fold(Protocol, Port, Instance, Host, Stats, node())).

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
%% time of setup
%% @end
%%%-----------------------------------------------------------------------------
-spec(find_push_stats([node()], {protocol(),sanitised_push()}) -> no_return()).
find_push_stats(Nodes,{Protocol, SanitisedData}) ->
    fold_through_meta(Protocol, SanitisedData, Nodes).


-spec(fold_through_meta(protocol(),sanitised_push(),[node()]) -> [push_arg()]).
fold_through_meta(Protocol, {{Port, Instance, Host}, Stats}, Nodes) ->
    fold_through_meta(Protocol,Port,Instance,Host,Stats,Nodes).
fold_through_meta(Protocol, Port, Instance, Host, Stats, Nodes) ->
    lists:map(fun(Node) ->
        fold(Protocol, Port, Instance, Host, Stats, Node)
             end, Nodes).

-spec(fold(protocol(),port(),instance(),host(),metrics(),node()) ->
    [push_arg()]).
fold(Protocol, Port, Instance, Host, Stats, Node) ->
    {Return, Port, Host, Stats, Node} =
        cluster_metadata:fold(
            fun
                ({{MProtocol, MInstance},   %% Key would be the same
                    [#{node := MNode,
                       port := MPort,
                       host := MHost,
                       stats := MStats} = MapValue]},

                    {Acc, APort, AHost, AStats, ANode}) %% Acc and Guard
                    when (APort     == MPort  orelse APort     == '_')
                    and  (AHost     == MHost  orelse AHost     == '_')
                    and  (ANode     == MNode  orelse ANode     == node())
                    and  (AStats    == MStats orelse AStats    == '_') ->
                    %% Matches all the Guards given in Acc
                    {[{{MProtocol,MInstance}, MapValue} | Acc],
                        APort, AHost, AStats,ANode};

                %% Doesn't Match Guards above
                ({_K, _V}, {Acc, APort, AServerIP,AStats,ANode}) ->
                    {Acc, APort, AServerIP,AStats,ANode}
            end,
            {[], Port, Host, Stats, Node}, %% Accumulator
            ?PUSH_PREFIX, %% Prefix to Iterate over
            [{match, {Protocol, Instance}}] %% Key to Object match
        ),
    Return.

