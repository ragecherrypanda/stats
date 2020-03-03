%%%-----------------------------------------------------------------------------
%%% @doc Testing functionality of the stats console @end
%%%-----------------------------------------------------------------------------
-module(stats_console_test).

-include_lib("eunit/include/eunit.hrl").
-include("stats_test.hrl").
-compile([export_all]).

%%% CONSOLE SPECIFIC MACROS

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Console Functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%=============================================================================
%%% stats_console.erl
%%%=============================================================================
%%% TEST DESCRIPTIONS %%%
stats_console_test_() ->
    Funs = [{"Testing stat_show",                              fun stat_show/1},
            {"Testing stat_info",                              fun stat_info/1},
            {"Testing stat_enable",                          fun stat_enable/1},
            {"Testing stat_disable",                        fun stat_disable/1},
            {"Testing stat_reset",                            fun stat_reset/1},
            {"Testing stat_metadata",                      fun stat_metadata/1},
            {"Testing stat_show /status=disabled",    fun stat_show_disabled/1},
            {"Testing stat_show /type=specific",      fun stat_show_specific/1},
            {"Testing stat_enabled (no persist)", fun stat_enable_no_persist/1},
            {"Testing stat_disable stat_disable",  fun stat_disable_disabled/1},
            {"Testing stat_reset",               fun reset_stat_non_existing/1},
            {"Testing reset_stat * 10",                  fun reset_stat_alot/1},
            {"Testing reload_metadata *10",         fun reload_metadata_alot/1}
            ],
    {inorder,
        {"Testing stats_console.erl functions for stats console funs",
            ?FOREACH(Funs)}}.

%%% TESTS %%%

stat_show(_) ->
    Arg1 = ["stats.test.**/type=duration"],
    Arg2 = ["stats.test.histogram.*/status=disabled"],
    Arg3 = ["stats.test.*.one.*/type=spiral"],
    Arg4 = ["stats.test.gauge.**"],
    Arg5 = ["histogram_test_nine_min"],

    Answer1 = [{[stat,test,duration,one], duration, enabled},
               {[stat,test,duration,two], duration, enabled}],
    Answer2 = [{[stat,test,histogram,ten],  histogram, disabled},
               {[stat,test,histogram,nine], histogram, disabled},
               {[stat,test,histogram,seven],histogram, disabled},
               {[stat,test,histogram,four], histogram, disabled},
               {[stat,test,histogram,two],  histogram, disabled}],
    Answer3 = [{[stat,test,spiral,one],       spiral, enabled},
               {[stat,test,spiral,one,one],   spiral, enabled},
               {[stat,test,spiral,one,two],   spiral, enabled},
               {[stat,test,spiral,one,three], spiral, enabled},
               {[stat,test,spiral,one,four],  spiral, enabled},
               {[stat,test,spiral,one,five],  spiral, enabled}],
    Answer4 = [{[stat,test,gauge,one],   gauge, enabled},
               {[stat,test,gauge,two],   gauge, enabled},
               {[stat,test,gauge,three], gauge, enabled},
               {[stat,test,gauge,four],  gauge, enabled},
               {[stat,test,gauge,five],  gauge, enabled}],
    Answer5 = [{[stat,test,histogram,nine],histogram,disabled}],

    ShowStat1 = stats_console:show_stat(Arg1),
    ShowStat2 = stats_console:show_stat(Arg2),
    ShowStat3 = stats_console:show_stat(Arg3),
    ShowStat4 = stats_console:show_stat(Arg4),
    ShowStat5 = stats_console:show_stat(Arg5),

    ?assert(ShowStat1==Answer1),
    ?assert(ShowStat2==Answer2),
    ?assert(ShowStat3==Answer3),
    ?assert(ShowStat4==Answer4),
    ?assert(ShowStat5==Answer5).

stat_info(_) -> ok.


stat_enable(_) -> ok.


stat_disable(_) -> ok.


stat_reset(_) -> ok.


stat_metadata(_) -> ok.


stat_show_disabled(_) -> ok.


stat_show_specific(_) -> ok.


stat_enable_no_persist(_) -> ok.


stat_disable_disabled(_) -> ok.


reset_stat_non_existing(_) -> ok.


reset_stat_alot(_) -> ok.


reload_metadata_alot(_) -> ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Profile Functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%=============================================================================
%%% stats_profile.erl
%%%=============================================================================



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Push Functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%=============================================================================
%%% stats_push.erl
%%%=============================================================================


%%%=============================================================================
%%% stats_push_sup.erl
%%%=============================================================================


%%%=============================================================================
%%% stats_push_tcp_serv.erl
%%%=============================================================================


%%%=============================================================================
%%% stats_push_udp_serv.erl
%%%=============================================================================


