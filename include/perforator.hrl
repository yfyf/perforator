-define(TEST_FUN_SUFFIX, "_perf").
-define(GENERATOR_FUN_SUFFIX, "_perf_").


-ifdef(TEST).
% This helps for faster unit tests.
-define(GC_SLEEP, 0).
-define(DEFAULT_SLEEP_TIME, 50).
-define(DEFAULT_RUN_COUNT, 3).
-else.
-define(GC_SLEEP, 500).
-define(DEFAULT_RUN_COUNT, 5).
-define(DEFAULT_SLEEP_TIME, 300).
-endif.
