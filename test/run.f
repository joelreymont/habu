\ run.f - native test suite entry.

require test/run-lib.f
require test/json-read-perf-phase.f

TR-PREPARE
TR-EARLY-EXTERNAL-START
require test/run-resident.f
TR-DAG-RUN-REST
JSON-READ-PERF-PHASE:START
TR-COMPLETE
