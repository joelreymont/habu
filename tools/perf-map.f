\ perf-map.f - the `perf script` address filter, as a command.
\
\ See tools/perf-map-core.f for what it does and why. This file is the entry
\ point; the package is separate so tools/perf-map-test.f can drive one line at
\ a time without a pipe.
require tools/perf-map-core.f

PERF-MAP:MAIN
