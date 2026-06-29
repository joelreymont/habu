\ gate-engine.f - entry wrapper for engine and public hb gate checks.
\
\ Load after test/gate-common.f, lib/memory.f, lib/build.f,
\ lib/codesign.f, and tools/build-fixpoint.f.

include test/gate-engine-lib.f

' GE-MAIN catch s" native engine gate" GE-THROW-REPORT
