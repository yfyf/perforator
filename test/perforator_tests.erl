-module(perforator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("perforator.hrl").
-include("eunit_utils.hrl").
-include("log_utils.hrl").

-define(TEST_FUN_SLEEP, 100). % 100ms
% The maximum duration that a test case which does nothing can take in the
% context of a perforator test.
-define(NULL_TESTCASE_MAX_TIME, 50). % 50ms

-compile(export_all).

perforator_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            %{"Fun in {setup, ..} corret name ", fun test_fun_in_setup_name/0}
            {"Correct duration", fun test_correct_duration/0},
            {"{foreach, ..} fixture test", fun test_foreach_fixture/0},
            {"{repeat, ..} fixture test", fun test_repeat_fixture/0}
        ]
    }.

setup() ->
    application:load(sasl),
    application:set_env(sasl, errlog_type, error),
    ?silent(perforator:ensure_deps_started()),
    perforator:ensure_deps_started(),
    ok.

cleanup(_) ->
    ?silent(perforator:stop_deps()),
    ok.

test_correct_duration() ->
    Test = {raw_fun, {?MODULE, named_fun_perf, 0}},
    Results = perforator:run_test(Test),
    ?assertMatch({named_fun_perf, _}, Results),
    lists:foreach(fun ({_Run, RunResults}) ->
        ?assertMatch({success, _}, RunResults),
        {success, Stats} = RunResults,
        Duration = proplists:get_value(duration, Stats),
        ?assertApprox(?TEST_FUN_SLEEP*1000, Duration)
    end, get_runs(Results)).

test_foreach_fixture() ->
    TestObj = {foreach, fun () -> ok end, fun (_) -> ok end, [
        fun () -> ok end, %% test case 1
        fun (_) -> ok end %% test case 2
    ]},
    Results = perforator:run_test(TestObj),
    ?assertEqual(2, length(Results)),
    Result1 = hd(Results),
    Result2 = hd(tl(Results)),
    Runs1 = get_runs(Result1),
    Runs2 = get_runs(Result2),
    ?assertEqual(?DEFAULT_RUN_COUNT, length(Runs1)),
    ?assertEqual(?DEFAULT_RUN_COUNT, length(Runs2)),
    CheckEverythingFun = fun (Runs) ->
        lists:foreach(fun (Run) ->
            {_, Rez} = Run,
            ?assertMatch({success, _}, Rez),
            {success, Stats} = Rez,
            Duration = proplists:get_value(duration, Stats),
            % the test case does nothing so should terminate immediatelly
            ?assert(Duration =< ?NULL_TESTCASE_MAX_TIME)
        end, Runs)
    end,
    CheckEverythingFun(Runs1),
    CheckEverythingFun(Runs2).

test_repeat_fixture() ->
    RunCount = 40,
    SleepTime = 1, % 1ms, the SleepTime is tested indirectly: if the test
    % doesn't time out, this means that the value was picked up.
    TestObj = {repeat, RunCount, SleepTime, fun () -> ok end},
    Result = perforator:run_test(TestObj),
    ?assertMatch({_, [_|_]}, Result),
    Runs = get_runs(Result),
    ?assertEqual(RunCount, length(Runs)),
    CheckEverythingFun = fun (Runs) ->
        lists:foreach(fun (Run) ->
            {_, Rez} = Run,
            ?assertMatch({success, _}, Rez),
            {success, Stats} = Rez,
            Duration = proplists:get_value(duration, Stats),
            % the test case does nothing so should terminate immediatelly
            ?assert(Duration =< ?NULL_TESTCASE_MAX_TIME)
        end, Runs)
    end,
    CheckEverythingFun(Runs).

%% @todo Fix + test this later
%test_fun_in_setup_name() ->
%    Fun = fun named_fun_perf/0,
%    ?debug("FunInfo: ~p~n", [erlang:fun_info(Fun)]),
%    {name, Name} = erlang:fun_info(Fun, name),
%    ExternalFun = erlang:make_fun(?MODULE, Name, 0),
%    ?debug("FunInfo2: ~p~n", [erlang:fun_info(ExternalFun)]),
%    TestObj = {setup, fun () -> ok end, fun (_) -> ok end, fun named_fun_perf/0},
%    Result = perforator:run_test(TestObj),
%    {TestName, TestData} = Result,
%    ?assertSublist( %% the name we get is f-d up, but you have to deal with it!
%        atom_to_list(named_fun_perf),
%        TestName
%    ).

%% ============================================================================
%% Perf test functions
%% ============================================================================

named_fun_perf() ->
    timer:sleep(?TEST_FUN_SLEEP).


%% ============================================================================
%% Helpers
%% ============================================================================

get_runs({_Name, Results}) ->
    proplists:get_value(runs, Results);
get_runs(Results) ->
    proplists:get_value(runs, Results).
