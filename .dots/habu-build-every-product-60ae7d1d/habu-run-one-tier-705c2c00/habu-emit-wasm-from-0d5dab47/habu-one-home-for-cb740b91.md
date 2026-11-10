---
title: One home for the facts t0-wasm.f and link-routines.f both state
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T19:22:00.035253+02:00"
---

Problem (feature review A, F3): CRASH-RC 134 is in src/habu/t0-wasm.f:29, src/arch/wasm/link-routines.f:18 and select.f:397; RDEPTH-AT and LDEPTH-AT in t0-wasm.f:492-493 and link-routines.f:167-168; OUT-CAP in link-routines.f:56, kernel.f and test/wasm/harness.f; the Wasm opcode bytes in t0-wasm.f:32-79 and link-routines.f:20-152; the checked-address test and the stack-room fault are written twice (CHECKED and ADDRESS-CHECK,; ROOM and STACK-ROOM,). A change in one file leaves the generator and the routines disagreeing with nothing to catch it; the depths are the worst case. Acceptance: CRASH-RC and OUT-CAP are WPROF constants; one file both require holds the opcode bytes and the two depth addresses; tier 1's copies (select.f, kernel.f) go with tier 1. Files: src/arch/wasm/profile.f, a new src/arch/wasm file, src/habu/t0-wasm.f, src/arch/wasm/link-routines.f, test/wasm/harness.f. Verify: engines converge; test/wasm/hb-build.f ok. Depends: the trap-fault dot (its cases cover the checks). Ownership: those files. Claim: unassigned.
