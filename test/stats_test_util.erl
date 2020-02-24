%%%-------------------------------------------------------------------
%%% @doc
%%% functions used in the test modules
%%% @end
%%%-------------------------------------------------------------------
-module(stats_test_util).

%% API
-ifdef(EUNIT).
-compile([export_all]).
-endif.

%%% Stat Generator Macros
-define(TEST_PREFIX,                                                      stat).
-define(TEST_APPS,                      [main,common,system,server,backend,ui]).
-define(TEST_CACHES,      [1000,2000,3000,4000,5000,6000,7000,8000,9000,10000]).
-define(TEST_STATUSES,                         [enabled,disabled,unregistered]).
-define(TEST_NAMES,        [stat,counter,active,list,down,cpu,mem,level,usage]).
-define(TEST_TYPES,[histogram, gauge, spiral, counter, duration, fast_counter]).
-define(HISTOGRAM_ALIAS,                     ['mean','max','99','95','median']).
-define(SPIRAL_ALIAS,                                          ['one','count']).
-define(DURATION_ALIAS,                            ['mean','max','last','min']).

-define(TEST_STAT_NUM,                                                    1000).

start_up_apps_and_deps() ->
    exometer:start(), % main stats
    application:ensure_started(cluster_metadata), % persistence
    stats_app:start(normal, ok).

random_stats() ->
    NumberOfStats = ?TEST_STAT_NUM,
    [stat_generator() || _ <- lists:seq(1,NumberOfStats)].


stat_generator() ->
    Prefix = ?TEST_PREFIX,
    RandomApp = pick_random(?TEST_APPS),
    NamesNumber = random_element_number(?TEST_NAMES),
    RandomName = [pick_random(?TEST_NAMES) || _ <- lists:seq(1,NamesNumber)],
    Stat = [Prefix, RandomApp | RandomName],
    RandomType = pick_random(?TEST_TYPES),
    RandomOptions = random_option(),
    RandomAliases = random_aliases(Stat, RandomType),
    {Stat, RandomType, RandomOptions, RandomAliases}.

random_option() ->
    RandomCache = {cache, pick_random(?TEST_CACHES)},
    RandomStatus = {status, [pick_random(?TEST_STATUSES)]},
    [RandomCache, RandomStatus].

random_aliases(Stat, histogram) -> [alias(Stat,Alias)||Alias<-?HISTOGRAM_ALIAS];
random_aliases(Stat, spiral) ->       [alias(Stat,Alias)||Alias<-?SPIRAL_ALIAS];
random_aliases(Stat, duration) ->   [alias(Stat,Alias)||Alias<-?DURATION_ALIAS];
random_aliases(_Stat, _Type) ->                                              [].

alias(Stat,Alias) ->
    Pre = lists:map(fun(S) -> atom_to_list(S) end, Stat),
    Pref = lists:join("_", Pre),
    Prefi = lists:concat(Pref++"_"++atom_to_list(Alias)),
    Prefix = list_to_atom(Prefi),
    {Alias, Prefix}.

random_element_number(List) -> rand:uniform(length(List)).

pick_random([]) ->
    pick_random([error]);
pick_random(List) ->
    Number = length(List),
    Element = rand:uniform(Number),
    element(Element, List).
