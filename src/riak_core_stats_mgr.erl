%%%-------------------------------------------------------------------
%%% @doc
%%% This module is for the administration of the stats within
%%% riak_core and other stats, in other riak apps.
%%%
%%% These _stat modules call into this module to REGISTER, READ,
%%% UPDATE, DELETE or RESET the stats.
%%% @end
%%%-------------------------------------------------------------------
-module(riak_core_stats_mgr).
-include("riak_stat.hrl").

%% API
-export([
    register/2,
    exometer_find_entries/2,
    aliases_prefix_foldl/0,
    alias/1,
    unregister/1,
    timestamp/0
]).


%%%-------------------------------------------------------------------
%% @doc
%% Use @see exometer:find_entries to get the name, type and status of
%% a stat given, fo all the stats that match the Status given put into
%% a list to be returned
%% @end
%%%-------------------------------------------------------------------
-spec(exometer_find_entries(metricname(), status()) -> listofstats()).
exometer_find_entries(Stats, Status) ->
    [lists:foldl(fun
                     ({Name, Type, EStatus}, Found)
                         when EStatus == Status orelse Status == '_' ->
                         [{Name, Type, Status} | Found];
                     (_, Found) -> Found % Different status
                 end,[], exometer_find_entries(Stat)) || Stat <- Stats].

exometer_find_entries(Stat) ->
    exometer:find_entries(Stat).







aliases_prefix_foldl() ->
    exometer_alias:prefix_foldl(<<>>,alias_fun(),orddict:new()).

-spec(alias(Group :: orddict:orddict()) -> ok | acc()).
alias(Group) ->
    lists:keysort(
        1,
        lists:foldl(
            fun({K, DPs}, Acc) ->
                case get_datapoint(K, [D || {D, _} <- DPs]) of
                    {ok, Vs} when is_list(Vs) ->
                        lists:foldr(fun({D, V}, Acc1) ->
                            {_, N} = lists:keyfind(D, 1, DPs),
                            [{N, V} | Acc1]
                                    end, Acc, Vs);
                    Other ->
                        Val = case Other of
                                  {ok, disabled} -> undefined;
                                  _ -> 0
                              end,
                        lists:foldr(fun({_, N}, Acc1) ->
                            [{N, Val} | Acc1]
                                    end, Acc, DPs)
                end
            end, [], orddict:to_list(Group))).



















