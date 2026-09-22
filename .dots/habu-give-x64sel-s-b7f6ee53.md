---
title: "Give X64SEL's call saves the residency mask"
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T05:52:21.275927+03:00"
---

Problem (measured by the x64-tail lane on engine 44a51834, no edit; probe /tmp/claude-1001/-home-joel-Work-habu/8eeb39b7-90d5-45fa-a940-514527337252/scratchpad/probe-tail.f): a 1-in/1-out X64ABI:TAIL routine selected by X64SEL is refused by the validator with E-A64RAV-DKEEP (-8611). X64SEL's CALL-SAVE (select-x64.f:622-631) has no residency mask and re-stores each argument into the very cell the entry dload read it from, which VDSTORE-CK (regalloc-verify.f:1746-1751, DKEEP-SAME) refuses by name; A64SEL's CALL-SAVE takes the mask (select.f:867-881) and EMIT-TAIL-CALL passes it (:2548-2556), which is why ARM64 tail routines validate live (test/compiler/native-tail.f). So every x86-64 routine ending in a tail call is refused today, and ordinary x86-64 call sites carry the same redundant stores. Acceptance: X64SEL's CALL-SAVE and EMIT-TAIL-CALL take a residency mask - decide whether to port A64SEL's residency (MASK-AT, MASK-FOLD-FOR, MASK-PRODUCER?, EMIT-MASKI) or to compute the call-site mask from what the entry loaded and nothing has since rewritten, after reading both; no store into a cell that still holds the value is emitted; a real X64ABI:TAIL 1-in/1-out routine allocates and validates in test/compiler/x64-regalloc.f (today -8611), and a call site with a resident argument emits no re-store in test/compiler/x64-select.f; ARM64 census byte-identical; three generations with cmp; test/run.f. Files: src/compiler/native/select-x64.f, test/compiler/x64-select.f, test/compiler/x64-regalloc.f. Depends: none. Ownership: hazel (x86-64 lowering, beside habu-lower-hir-to-6bf80d33). Claim: unassigned.
