%% @doc Various statistics functions
-module(perforator_stats).

-include("log_utils.hrl").

-export([
    means/1,
    mins/1,
    maxes/1 %% lame name, I know, but what-you-gonna-do
]).

%% @todo add specs and unit test new cases

means(MetricsList) ->
    metrics_map(
        fun (Values) -> lists:sum(Values) / length(Values) end,
        MetricsList).

maxes(MetricsList) ->
    metrics_map(fun lists:max/1, MetricsList).

mins(MetricsList) ->
    metrics_map(fun lists:min/1, MetricsList).

metrics_map(Fun, MetricsList) ->
    FlatMetrics = lists:append(MetricsList),
    Metrics = proplists:get_keys(FlatMetrics),
    lists:map(fun (Metric) ->
        Values = proplists:get_all_values(Metric, FlatMetrics),
        {Metric, map_or_na(Fun, Values)}
    end, Metrics).


map_or_na(Fun, Values) ->
    case lists:member('NA', Values) of
        true ->
            'NA';
        false ->
            Fun(Values)
    end.
