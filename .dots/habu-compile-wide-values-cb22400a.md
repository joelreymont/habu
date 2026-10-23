---
title: Compile wide values in a tier-0 does> clause
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:43.029007+03:00"
---

Problem: at tier 0 a wide (multi-cell) value in a does> clause compiles as one cell when the definer's head has no width facts; tier 1 is correct (2026-10-10: s-does-tr2 printed `3 7 2 1` at tier 0, `3 2 1 7` at tier 1). Cause recorded 2026-10-10 (carl), unverified since: the head and clause checks write one certificate buffer and pass 2 replays only the head's (src/habu/habu2.f C-CALL-CHECK-DOES, LOWER-TXN:FREEZE, EM-P2-TRIGGER). The original probes are gone: reproduce first, at tier 0 (`0 set-tier`) and tier 1 (`1 set-tier`) on ARM64. Tier 1 is the reference.
Acceptance: the clause moves the whole value at tier 0; the reproducer prints tier 1's output at both tiers. Verify: rebuild bin/hb per docs/gate.md; the reproducer as a native-suite row; `bin/hb --load test/run.f`; two-generation build. Files: src/habu/habu2.f, bootstrap/cg/forth.fs where the seed mirrors the path, the test row. Claim: unassigned.
