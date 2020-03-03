%%%-------------------------------------------------------------------
%%% @doc
%%% functions used in the test modules
%%% @end
%%%-------------------------------------------------------------------
-module(stats_test_util).
-include("stats_test.hrl").

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

stats() ->
    [   %% 5 x gauge stats
        {[stat,test,gauge,one], gauge},
        {[stat,test,gauge,two], gauge},
        {[stat,test,gauge,three], gauge},
        {[stat,test,gauge,four], gauge},
        {[stat,test,gauge,five], gauge},

        %% 10 histograms % randomly enabled or disabled on registration.
        {[stat,test,histogram,one],histogram,[{status, enabled}],
            [{max,histogram_test_one_max},      {min,histogram_test_one_min},
             {mean,histogram_test_one_mean},    {99,histogram_test_one_99},
             {95,histogram_test_one_95},        {60,histogram_test_one_60}]},
        {[stat,test,histogram,two],histogram,[{status, disabled}],
            [{max,histogram_test_two_max},      {min,histogram_test_two_min},
             {mean,histogram_test_two_mean},    {99,histogram_test_two_99},
             {95,histogram_test_two_95},        {60,histogram_test_two_60}]},
        {[stat,test,histogram,three],histogram,[{status, enabled}],
            [{max,histogram_test_three_max},    {min,histogram_test_three_min},
             {mean,histogram_test_three_mean},  {99,histogram_test_three_99},
             {95,histogram_test_three_95},      {60,histogram_test_three_60}]},
        {[stat,test,histogram,four],histogram,[{status, disabled}],
            [{max,histogram_test_four_max},     {min,histogram_test_four_min},
             {mean,histogram_test_four_mean},   {99,histogram_test_four_99},
             {95,histogram_test_four_95},       {60,histogram_test_four_60}]},
        {[stat,test,histogram,five],histogram,[{status, enabled}],
            [{max,histogram_test_five_max},     {min,histogram_test_five_min},
             {mean,histogram_test_five_mean},   {99,histogram_test_five_99},
             {95,histogram_test_five_95},       {60,histogram_test_five_60}]},
        {[stat,test,histogram,six],histogram,[{status, enabled}],
            [{max,histogram_test_six_max},      {min,histogram_test_six_min},
             {mean,histogram_test_six_mean},    {99,histogram_test_six_99},
             {95,histogram_test_six_95},        {60,histogram_test_six_60}]},
        {[stat,test,histogram,seven],histogram,[{status, disabled}],
            [{max,histogram_test_seven_max},    {min,histogram_test_seven_min},
             {mean,histogram_test_seven_mean},  {99,histogram_test_seven_99},
             {95,histogram_test_seven_95},      {60,histogram_test_seven_60}]},
        {[stat,test,histogram,eight],histogram,[{status, enabled}],
            [{max,histogram_test_eight_max},    {min,histogram_test_eight_min},
             {mean,histogram_test_eight_mean},  {99,histogram_test_eight_99},
             {95,histogram_test_eight_95},      {60,histogram_test_eight_60}]},
        {[stat,test,histogram,nine],histogram,[{status, disabled}],
            [{max,histogram_test_nine_max},     {min,histogram_test_nine_min},
             {mean,histogram_test_nine_mean},   {99,histogram_test_nine_99},
             {95,histogram_test_nine_95},       {60,histogram_test_nine_60}]},
        {[stat,test,histogram,ten],histogram,[{status, disabled}],
            [{max,histogram_test_ten_max},      {min,histogram_test_ten_min},
             {mean,histogram_test_ten_mean},    {99,histogram_test_ten_99},
             {95,histogram_test_ten_95},        {60,histogram_test_ten_60}]},

        %% 15 spirals
        {[stat,test,spiral,one],        spiral, [{cache, 1000}],
            [{one, spiral_one_one},         {count, spiral_one_count}]},
        {[stat,test,spiral,two],        spiral, [{cache, 1000}],
            [{one, spiral_two_one},         {count, spiral_two_count}]},
        {[stat,test,spiral,three],      spiral, [{cache, 1000}],
            [{one, spiral_three_one},       {count, spiral_three_count}]},
        {[stat,test,spiral,four],       spiral, [{cache, 1000}],
            [{one, spiral_four_one},        {count, spiral_four_count}]},
        {[stat,test,spiral,five],       spiral, [{cache, 1000}],
            [{one, spiral_five_one},        {count, spiral_five_count}]},
        {[stat,test,spiral,six],        spiral, [{cache, 1000}],
            [{one, spiral_six_one},         {count, spiral_six_count}]},
        {[stat,test,spiral,seven],      spiral, [{cache, 1000}],
            [{one, spiral_seven_one},       {count, spiral_seven_count}]},
        {[stat,test,spiral,eight],      spiral, [{cache, 1000}],
            [{one, spiral_eight_one},       {count, spiral_eight_count}]},
        {[stat,test,spiral,nine],       spiral, [{cache, 1000}],
            [{one, spiral_nine_one},        {count, spiral_nine_count}]},
        {[stat,test,spiral,ten],        spiral, [{cache, 1000}],
            [{one, spiral_ten_one},         {count, spiral_ten_count}]},
        {[stat,test,spiral,one,one],    spiral, [{cache, 1000}],
            [{one, spiral_one_one_one},     {count, spiral_one_one_count}]},
        {[stat,test,spiral,one,two],    spiral, [{cache, 1000}],
            [{one, spiral_one_two_one},     {count, spiral_one_two_count}]},
        {[stat,test,spiral,one,three],  spiral, [{cache, 1000}],
            [{one, spiral_one_three_one},   {count, spiral_one_three_count}]},
        {[stat,test,spiral,one,four],   spiral, [{cache, 1000}],
            [{one, spiral_one_four_one},    {count, spiral_one_four_count}]},
        {[stat,test,spiral,one,five],   spiral, [{cache, 1000}],
            [{one, spiral_one_five_one},    {count, spiral_one_five_count}]},

        %% 5 counters
        {[stat,test,counter,one],   counter, [], [{count,  one_count}]},
        {[stat,test,counter,two],   counter, [], [{count,  two_count}]},
        {[stat,test,counter,three], counter, [], [{count,three_count}]},
        {[stat,test,counter,four],  counter, [], [{count, four_count}]},
        {[stat,test,counter,five],  counter, [], [{count, five_count}]},

        %% 2 duration
        {[stat,test,duration,one], duration},
        {[stat,test,duration,two], duration}
    ].




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
