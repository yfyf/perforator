-module(perforator_metrics_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("log_utils.hrl").

%% ============================================================================
%% Unit Test descriptions
%% ============================================================================

perforator_metrics_test_() ->
    {foreach,
        fun() ->
            application:load(sasl),
            application:set_env(sasl, errlog_type, error),
            application:start(sasl),
            application:start(os_mon)
        end,
        fun(_) ->
            ?silent(application:stop(os_mon)),
            ?silent(application:stop(sasl))
         end,
        [
            {"Collector process test", fun test_collector/0},
            {"Dying collect process test", fun test_dead_collector/0},
            {"Agregate stats test", fun test_agregate/0}
        ]
    }.

%% ============================================================================
%% Actual tests
%% ============================================================================

test_collector() ->
    Pid = perforator_metrics:init_collect(),
    timer:sleep(1000),
    {ok, Return} = perforator_metrics:retrieve(Pid),
    ?assertEqual(2, length(Return)).

test_dead_collector() ->
    Pid = spawn(fun() -> timer:sleep(1) end),
    ?assertEqual({error, unable_to_retrieve_stats},
        perforator_metrics:retrieve(Pid)).

test_agregate() ->
    ReadList = [[{foo, 2}, {bar, 15}], [{foo, 6}, {bar, 1}]],
    ExpectedReturn = [
        {mean, [{foo, 4.0}, {bar, 8.0}]},
        {min, [{foo, 2}, {bar, 1}]},
        {max, [{foo, 6}, {bar, 15}]}
    ],
    ?assertEqual(ExpectedReturn, perforator_metrics:agregate(ReadList)).
