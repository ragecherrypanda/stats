%%%-----------------------------------------------------------------------------
%%% @doc Testing all the Stats @end
%%%-----------------------------------------------------------------------------
-module(stats_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assert(true).

%%% EUNIT Macros
-define(SETUP(Fun),                  {setup, fun setup/0, fun cleanup/1, Fun }).
-define(FOREACH(Funs),             {foreach, fun setup/0, fun cleanup/1, Funs}).

-define(SPAWN(Test),                                      {spawn,        Test}).
-define(TIMEOUT(Test),                                    {timeout, 120, Test}).
-define(IN_ORDER(Test),                                   {inorder,      Test}).
-define(IN_PARALLEL(Test),                                {inparallel,   Test}).

-define(SETUP_TEST(Desc, Test),             {Desc, ?SETUP(fun(_) -> Test end)}).

%%% Meck Macros
-define(new(Mod),                   meck:new(Mod)).
-define(expect(Mod,Fun,Func),       meck:expect(Mod,Fun,Func)).
-define(expect(Mod,Fun,Val,Func),   meck:expect(Mod,Fun,Val,Func)).
-define(unload(Mod),                meck:unload(Mod)).


setup()->
    ok.

cleanup(_PidorPids) ->
    ok.


%%%-----------------------------------------------------------------------------
%%% @doc individual module EUNIT Tests: @end
%%%-----------------------------------------------------------------------------

%%%=============================================================================
%%% stats.erl
%%%=============================================================================

%% todo: re_register



%%%=============================================================================
%%% stats_persist.erl
%%%=============================================================================
