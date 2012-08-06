-module(perforator_stats_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Unit Test descriptions
%% ============================================================================

perforator_stats_test_() ->
    {foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            {"Mean value calculation test", fun test_means/0},
            {"Min value calculation test", fun test_mins/0},
            {"Max value calculation test", fun test_maxes/0}
        ]
    }.

%% ============================================================================
%% Actual tests
%% ============================================================================

test_means() ->
    Reads = [
        [{cpu_util, N}, {cpu_load, N+3}] || N <- lists:seq(1, 3)
    ],
    ExpectedAverage = [{cpu_load, 5.0}, {cpu_util, 2.0}],
    ?assertEqual(ExpectedAverage, perforator_stats:means(Reads)).

test_mins() ->
    Reads = [
        [{a, 1}, {b, 5}], [{a, 3}, {b, 3}]
    ],
    Expectedmins = [{a, 1}, {b, 3}],
    ?assertEqual(Expectedmins, perforator_stats:mins(Reads)).

test_maxes() ->
    Reads = [
        [{a, 1}, {b, 5}], [{a, 3}, {b, 3}]
    ],
    Expectedmaxes = [{a, 3}, {b, 5}],
    ?assertEqual(Expectedmaxes, perforator_stats:maxes(Reads)).
