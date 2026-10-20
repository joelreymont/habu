---
title: Pair stack pop loads
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T01:56:08.943517+03:00"
---

Bounded improvement within data-stack traffic issue93fe1f36: existing emitted DPOP followed by DLOAD at +8 can become one pre-indexed LDP with the same reads and writeback. Independent Astra design found88 adjacent candidates in installed Dots (maximum352 code bytes) and824 in installed hb (maximum3296 bytes); no built before/after gain is claimed. Existing SSA residency, ordinary pairing and pointer cancellation already exist, so do not duplicate them or redesign the stack ABI. Extend existing ARM64 pair encoder/planner/emitter only; preserve same-block/origin, register-file, distinct destinations, base-register and barrier restrictions. Write focused existing E2E cases and independently assembled encoding before implementation. Produce same-source executable behavior and actual code/image measurements, distinguishing fewer instructions from unchanged memory transfers. Shared codegen requires matching native gate, generation convergence and independent Astra review. Broader93fe1f36 stays open; this is not a15–20KB saving. No app edits, codec, IR/format or ABI change.

Same-source ordinary two-cell consumers on4849828a versus7cc7: TAKE2 drops14→13 instructions/56→52bytes and TAKE-CALL11→10/44→40 by replacing two scalar x19 loads with one preindexed pair. Both execute correctly. MIX and SUM remain byte-identical; no claim that pairing reduces the number of memory cells read.
