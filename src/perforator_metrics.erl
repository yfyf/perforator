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
            spawn(?MODULE, Collector, [])
        end,
        ?COLLECTORS
    ).

retrieve(Pids) when is_list(Pids) ->
    %% @todo: this should be performed in parallel. In general, refactoring awaits.
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

%% @doc Returns average CPU utilization ([should be] the same as returned by the UNIX
%% `top` command)
collect_cpu_util() ->
    % first util call has no meaning, so we discard it.
    cpu_sup:util(),
    collect_cpu_util([], ?COLLECT_INTERVAL).

collect_cpu_util(Stats, SleepTime) ->
    receive {retrieve, Sender} ->
        % Divide by 256 to match what people are used to seeing in `top` output.
        % http://www.erlang.org/doc/man/cpu_sup.html
        Result = case (Stats == []) or (cpu_sup:util() == 0) of
            true -> % cpu_sup returns if 0 if result is unavailable
                'NA';
            false ->
                (lists:sum(Stats) / 256) / length(Stats)
        end,
        Sender ! {cpu_util, Result}
    after SleepTime ->
        collect_cpu_util([cpu_sup:util()|Stats], SleepTime)
    end.

%% @doc Returns total memory used by the Erlang VM in KiloBytes.
collect_vm_memory() ->
    collect_vm_memory([erlang:memory(total)], ?COLLECT_INTERVAL).
collect_vm_memory(Stats, SleepTime) ->
    receive {retrieve, Sender} ->
        Sender ! {vm_memory, (lists:sum(Stats) / 1024) / length(Stats)}
    after SleepTime ->
        collect_vm_memory([erlang:memory(total)|Stats], SleepTime)
    end.
