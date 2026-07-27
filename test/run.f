\ run.f - native test suite entry.
\
\ This file stays outside package TEST-RUN. One consumer import spans the
\ ordered runner calls and the late resident require; the required file opens
\ and closes its package definition scope without closing this using scope.

require test/run-lib.f
require test/json-read-perf-phase.f

using TEST-RUN
PREPARE
EARLY-EXTERNAL-START
require test/run-resident.f
DAG-RUN-REST
JSON-READ-PERF-PHASE:START
COMPLETE
;using
