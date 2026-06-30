\ run.f - native test suite entry.

require test/run-lib.f

TR-PREPARE
TR-EARLY-EXTERNAL-START
require test/run-resident.f
TR-DAG-RUN-REST
TR-COMPLETE
