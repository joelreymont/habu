\ perf-map.f - the `perf script` address filter, as a command. The filter lives in
\ tools/perf-map-core.f, in its own package so tools/perf-map-test.f drives one
\ line at a time without a pipe.
require tools/perf-map-core.f

PERF-MAP:MAIN
