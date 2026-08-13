---
title: Thread parked values through joins
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-13T16:52:48.011743+02:00\""
---

Full context: finishes habu-lower-native-return-92993f27, whose lane landed the straight-line half (WIP commit in .jj-ws/habu-rstack-design). >r/r>/r@ compile as compile-time transfers between the data vector and a new RETURN vector in src/compiler/native/elaborate.f; measured through NMIGRATE:MEASURE-HELD, >r/r@/2>r left E-HIR-UNMODELED and three-deep nesting compiles. WHAT IS MISSING, exactly: the parked values ride the data vector across a seam (R-SPILL before EDGE-STAGE in TERM-BR-H, R-FILL from ARG-R@ in OPEN-ARGS-H), so the FIRST edge into a join states a width that includes them - but the join OPENERS still pass a width computed from the DATA-only frame depth. Nine call sites of OPEN-ARGS/OPEN-ARGS-H pass d, w, xd or 't CS-DEPTH@ t CS-W@ +'; the control frame (CS-*) records no parked depth; and the arm checks (ARM-WIDTH-CK, ARM-RESHAPE, ARM-GLUE) compare arm depths against the frame's data depth. Work: add a parked-depth field to the control frame, add it at the nine openers, extend the arm checks, then the call seam (CALL-OPERANDS+/CALL-CLOSE, parked values must survive a call - they go in the FRONT group with the counters and locals, never after the arguments) and the return check (the return vector must be empty at hir.return; the checker already proved it). MEASURED TODAY: branch, split, counted loop, loop-carried and begin/while all refuse E-NELAB-JOIN; straight-line, pair forms and three-deep nesting compile; if/loop bodies WITHOUT the return stack are unaffected (native-elaborate, native-hir, native-do, native-inline, native-match, native-clobber, native-trap, native-defer all green). Acceptance: the differential shapes in the lane's /tmp/hb-rstack/diff.f answer bit-for-bit against the engine, and the census re-run shows the 16 >r first-refusals leaving E-HIR-UNMODELED without arriving at a different refusal. Files: src/compiler/native/elaborate.f.

Claim: agent=rstack-join workspace=.jj-ws/habu-rstack-design
