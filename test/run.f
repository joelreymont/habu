\ run.f - native test suite entry.
\
\ This file stays OUTSIDE package TEST-RUN on purpose. It interleaves a
\ `require` with the calls that have to run before that file loads, and a file
\ loaded while a package is open inherits that scope, so it could not open a
\ package of its own. This file defines nothing, so it simply names the
\ runner's words across the package boundary.

require test/run-lib.f
require test/json-read-perf-phase.f

TEST-RUN:PREPARE
TEST-RUN:EARLY-EXTERNAL-START
require test/run-resident.f
TEST-RUN:DAG-RUN-REST
JSON-READ-PERF-PHASE:START
TEST-RUN:COMPLETE
