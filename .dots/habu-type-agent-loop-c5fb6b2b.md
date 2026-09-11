---
title: Type agent loop transitions
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:21:49.883103+02:00"
---

maki/db/agent-loop.f:100-156 and :180-273 defines five SC-* step outcomes and five RN-* termination reasons as raw integers. GATE, APPLY, and PROPOSE-APPLY return n; STEP compares only terminal codes and silently treats every remaining value as a measurable committed/rejected step. Run state is split across generic RS-DONE and RS-REASON, with -1 meaning promoted; PROMOTE/BLOCK write the cells separately and RESULT reconstructs a variant later. The checker therefore permits step/reason/metric swaps and represents impossible done/reason combinations. Replace step codes with a closed ENUM, make GATE/APPLY/PROPOSE-APPLY return it, and use exhaustive MATCH in STEP. Replace RS-DONE plus RS-REASON with one payload-bearing ENUM run-state (running | promoted | blocked(blocked-reason)) stored in TYPED-VARIABLE; DRIVE/RESULT transition and match that value directly. Target the specified hard-cutover ENUM surface after its native lowering lands; do not add another legacy SUMTYPE declaration. Coordinate the existing Maki declaration cutover for loop-result without duplicating its syntax migration. Preserve mutation, budgeting, idempotency, progress, journal, and deterministic replay behavior. Add checker negatives for step/reason/metric and foreign-enum swaps; transition-table tests cover every step outcome, initial/terminal states, immediate promotion, each block reason, and prove no default fallthrough or split-state observation. Measure JIT/DATA/CODELEN and loop throughput before/after; require no unexplained growth. Files: maki/db/agent-loop.f and focused tests. Verify focused DB/agent-loop suites, Maki, typed-local diff, type/package/host/dot lints, fixpoint, and full native gate. Ownership: controller transition/state typing only.
