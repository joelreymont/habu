---
title: Split call-crossed values around the loop
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T17:03:15.149470+02:00"
---

CALL-PRESSURE (corpus 4) refuses E-A64RA-SPILL because elaborate.f CROSS-STEP threads a call-surviving local through every loop block (LOCAL-ARGS+) and the call's operand list (CALL-OPERANDS+); the exclusions from MB-SPILLABLE? originate there, upstream of the allocator (five allocator-side mutations measured, none moves the refusal; tools/codegen-spill-probe.f is the merged regression instrument pinning all eight facts).

PROVEN FIX, ONE LINE: under a flag, CROSS-L answers nought — threading suppressed; flag on, the shape compiles rc 0 with answers bit-identical to the engine incl. MAX-INT/MIN-INT. The 2223 operand-list hazard is already enforced twice downstream (regalloc.f MB-CROSSES?/MB-FORBID; regalloc-verify.f:523-538 independently), so no mark, no mandatory spill, no validator extension is needed.

BLOCKED BY THE CUT: the trigger must be a retry (attempt normally; on E-A64RA-SPILL re-run with the flag), and the bridge cannot host one — evaluation and publication are fused in the engine's EV (migrate.f RECORD), so a second attempt needs a second builder+model and either re-publishes or demands multi-function modules. The sole-compiler pipeline (habu-cut-colon-compilation-a5aa3f1f) separates compile-attempt from publish, making the retry cheap; it carries the requirement. Until then CALL-PRESSURE keeps its honest gap line. PRESSURE-LOOP is separate (loop-invariant loads; habu-rematerialize-the-loop-1faad3e1).
