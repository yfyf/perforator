%% @doc Module for collecting metrics during test execution.

-module(perforator_metrics).

-compile(export_all).
-export([
    init_collect/0,
    retrieve/1
]).

-define(COLLECT_INTERVAL, 300).
-define(MAX_RETRIEVE_WAIT, ?COLLECT_INTERVAL*3).
-define(COLLECTORS,
        [collect_cpu_util, collect_vm_memory]).


init_collect() ->
    lists:map(
        fun(Collector) ->
            spawn_link(?MODULE, Collector, [])
        end,
        ?COLLECTORS
    ).

retrieve(Pids) when is_list(Pids) ->
    {ok, lists:map(fun retrieve/1, Pids)};

retrieve(Pid) ->
    Pid ! {retrieve, self()},
    receive
        {Tag, Stat} when is_atom(Tag)->
            {Tag, Stat}
    after ?MAX_RETRIEVE_WAIT ->
        {error, unable_to_retrieve_stats}
    end.

%% ============================================================================
%% Collector processes
%% ============================================================================
% @todo Create a behaviour and move these to separate modules

collect_cpu_util() ->
    collect_cpu_util([cpu_sup:util()], ?COLLECT_INTERVAL).

collect_cpu_util(Stats, SleepTime) ->
    receive {retrieve, Sender} ->
        Sender ! {cpu_util, lists:sum(Stats) / length(Stats)}
    after SleepTime ->
        collect_cpu_util([cpu_sup:util()|Stats], SleepTime)
    end.

collect_vm_memory() ->
    collect_vm_memory([erlang:memory(total)], ?COLLECT_INTERVAL).
collect_vm_memory(Stats, SleepTime) ->
    receive {retrieve, Sender} ->
        Sender ! {vm_memory, lists:sum(Stats) / length(Stats)}
    after SleepTime ->
        collect_vm_memory([erlang:memory(total)|Stats], SleepTime)
    end.
