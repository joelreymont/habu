---
title: Remove redundant frame shuffling
status: active
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.677506+03:00"
---

Otto’s codegen report measures29,552 bytes of frame-slot traffic including1,186 adjacent copies (9,488 bytes). Establish which copies are redundant using compiler facts, not instruction-pattern deletion alone; preserve aliasing, wide values, spill/call and failure behavior. Estimated10–13KB savings are unverified. Acceptance: same-source executable behavior and emitted-code/size measurement; affected native E2E, full native gate, convergence and Astra review. No compression or application edits.

The rebuilt ed94c47b product passes native-wide-frame, native-loop-frame-order, native-tail, native-chain, native-fused-moves and native-identity-spill. The corrected frame-slot E2E produces and runs a native executable and retains the exact 16-byte output artifact frame-slots: ok followed by LF. Its six-word census remains 612 bytes, 153 instructions and 18 calls with both the prior donor and this candidate. The broader frame-traffic reduction remains unmeasured.

Same-source bounded FRAME-SLOT-IMAGE:BB JIT comparison: qualified4849828a emits43 instructions/172bytes; donor7cc7 emits39/156, removing three redundant frame loads and one store. This establishes one affected routine, not the historical whole-application19percent estimate.

Broader retained same-source Dots pair census now finds distinct-slot adjacent copies1405 to106 (10392 fewer code bytes), and SP transfer instructions9872 to6732 (12560 fewer bytes). Candidate is a7c8b6f0/engine87efab1e; exact baseline D2 source/engine provenance is unavailable, so these are integrated-pair results rather than per-patch attribution. Runtime output/status/stderr agree in the existing paired Dots workload. The reduced wide-loop MAP-POINT shape also fails public600 with -8522 and passes current source/native compilation; attribution to immutable-local SSA4fbc381d remains an inference. Final combined native/convergence qualification is pending.

Status 2026-10-21: both changes are on master by content (src/compiler/native/regalloc.f SH-* frame-slot sharing; src/compiler/native/emit.f stack-move merge across O-LINKLOAD/O-RELEASE); the original commits 3f5536f0 and bc89a2cb are not master ancestors. Remaining acceptance: independent review, then a measurement against a baseline with exact provenance (the old 1405->106 adjacent-copy census had none). Claim: agent=carl workspace=none (review is read-only).
