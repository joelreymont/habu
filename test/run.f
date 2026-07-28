\ run.f - native test suite entry.
\
\ This file stays outside package TEST. Its runner calls stay qualified because
\ importing all TEST publics across the resident require would expose RESET and
\ RUN to child files that own those names.

require test/run-lib.f
require test/json-read-perf-phase.f

TEST:PREPARE
TEST:EARLY-EXTERNAL-START
require test/run-resident.f
TEST:DAG-RUN-REST
JSON-READ-PERF-PHASE:START
TEST:COMPLETE
