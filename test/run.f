\ run.f - native test suite entry.
\
\ This file stays outside package TEST. Its runner calls stay qualified because
\ importing all TEST publics across the resident require would expose RESET and
\ RUN to child files that own those names.

require test/run-lib.f
require test/json-read-perf-phase.f
require tools/codegen-compare-cabi.f

\ Map the clang reference column here, in the process the gate was exec'd into,
\ before a single worker is forked. dyld is not fork-safe: asking it to load an
\ image that is not already mapped faults INSIDE dyld in a forked child, and
\ every process below this one is a fork of it -- the pool workers, and the
\ members they fork in turn. Two codegen-compare members reach that column and
\ exited 134 for exactly this reason until the mapping moved up here; they now
\ inherit it. On a host with no C compiler this builds and maps nothing and
\ those members report the column absent, as they do standalone.
\ Ownership and the refusal that keeps this honest: tools/codegen-compare-cc.f,
\ THE OWNER OF THE TREE.
CODEGEN-CABI:PREPARE drop

TEST:PREPARE
TEST:EARLY-EXTERNAL-START
require test/run-resident.f
TEST:DAG-RUN-REST
JSON-READ-PERF-PHASE:START
TEST:COMPLETE
