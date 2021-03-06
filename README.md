Perforator
=====

Perforator is a (E)unit-testing style performance testing tool.

Although it can be used as a standalone tool, to unleash it's full power
use it with the Perforator CI server. Check it out here:

https://github.com/brb/perforator-ci

[Travis-CI](http://travis-ci.org/yfyf/perforator) ::
 ![Build Status](https://secure.travis-ci.org/yfyf/perforator.png "Build status of Perforator")

Usage
-----

For enhanced comfort Perforator ships with a rebar plugin,
add this baby to your `rebar.config`:

(_Note: the plugin doesn't seem to be working with the newest rebar
version, this will be fixed shortly, but in the mean time use version
[2.0.0](https://github.com/basho/rebar/commit/1c98f6ccd4adc915167d4302d732d79e4da3d390)
which is guaranteed to work_)

``` erlang
{plugins, [perforator_rebar_plugin]}.
{deps, [
    {perforator, ".*", {git, "https://github.com/yfyf/perforator.git", "master"}}
]}
```

The testing goes like this:

* Write some `*_perf.erl` modules and put them in your `tests/` directory.
* Run `./rebar perf skip_deps=true'
* Explore the wonderful results written in `.perf/`

_perf.erl modules
-----

`_perf.erl` modules are supposed to be very similar (in terms of syntax) to
EUnit's test modules.

Test objects can be:
* simple:
```test_case_perf() -> timer:sleep(100).``` functions.
* EUnit style fixtures:
```test_generator_perf_() -> {setup, Setup, Cleanup, TestObj}.```
* for more examples check out `test/sample_lists_perf.erl` module.

Note #1: the `_perf_()`, `_perf()` suffixes, they are kind of the same as EUnit's
`_test()` and `_test_()` ones. The main difference between EUnit is that you
don't need to return a fun everywhere.
Note #2: since there are no parse transforms involved, you must export the
`*_perf[_]()` functions! `-compile(export_all).` is justifiable here.


Notes on test runs and statistics
----

Each test case is being run 5 times with 500ms sleeps (it's hardcoded for
now, sorry!) and averages are being calculated for the statistics to be more
meaningful.

Some of the statistics gathered are rather sloppy, but at least the duration is
tracked pretty tightly.
