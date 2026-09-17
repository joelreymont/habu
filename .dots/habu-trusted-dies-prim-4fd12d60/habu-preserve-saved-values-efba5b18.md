---
title: Preserve saved values after BEGIN UNTIL calls
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-09-11T16:50:23.160099+03:00\\\""
closed-at: "2026-09-11T17:21:55.825351+03:00"
close-reason: Reviewed19d081a6 integrated as9e6e770d; exact UNTIL/local/permutation and identity/shared-source tests pass, along with combined allocator and return-stack suites. Full compiler selfbuild tracked separately.
---

Owner: /root/compiler_xhigh_review (Astra), separate workspace based on cfcf1baf. Rowan KEEP review BB 20260911-133339.417-rowan-3be9 found this additional correctness blocker.

Reproducer: RV:CALLEE ( n -- n ) is 1+; `: A3 ( n n -- n ) >r begin RV:CALLEE dup 1000 > until r> + ;` and a typed-local equivalent fail E-A64RA-EDGE (-8507). Hidden value remains live at the exit after the backedge.

Diagnosis: selector EMIT-BR snapshots an identity edge operand that is already the destination block argument, creating an overlapping allocation class member. Prototype in src/compiler/native/select.f adds EDGE-STAYS? and omits both copies only for identity edges; nonidentity and permutation snapshots remain. Original reducer now returns 1043 in 0.298 s. Temporary regalloc diagnostics removed.

Acceptance: add returning UNTIL, local, and mixed-permutation regressions to native-loop-frame-order.f; run native-loop and native-regalloc negative controls; freeze source and matching binary; independent Astra review before integration. Frozen candidate 19d081a6558d930157a7770f7878751c522b8dee on cfcf1baf in cedar-spill-trap; independent review assigned to check_api. Matched binary SHA256 703484fae74772ad054eab8454c7f04418e6d9c926b00270b75df666fa42e09d. Six new runtime cases plus native-loop-frame-order (5.884 s), native-loop (12.299 s), native-select (12.375 s), native-regalloc (16.977 s) pass, including MB-EDGE-CLASH and malformed-edge negatives. Not integrated yet.


Update 2026-09-11 13:59 UTC: Candidate 19d081a6 independently cleared by check_api, including an extra identity-plus-shared-source control (44 and 25). Integrated as 9e6e770d. Exact SSA identity assignments only are omitted; nonidentity snapshot and assignment stages remain. Combined compiler validation pending.
