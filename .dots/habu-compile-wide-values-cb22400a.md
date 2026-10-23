---
title: Compile wide values in a tier-0 does> clause
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:43.029007+03:00"
---

Problem: at tier 0 a wide (multi-cell) value in a does> clause compiles as one cell when the definer's head has no width facts; tier 1 is correct (2026-10-10: s-does-tr2 printed `3 7 2 1` at tier 0, `3 2 1 7` at tier 1). Cause recorded 2026-10-10 (carl), unverified since: the head and clause checks write one certificate buffer and pass 2 replays only the head's (src/habu/habu2.f C-CALL-CHECK-DOES, LOWER-TXN:FREEZE, EM-P2-TRIGGER). The original probes are gone: reproduce first, at tier 0 (`0 set-tier`) and tier 1 (`1 set-tier`) on ARM64. Tier 1 is the reference.
Acceptance: the clause moves the whole value at tier 0; the reproducer prints tier 1's output at both tiers. Verify: rebuild bin/hb per docs/gate.md; the reproducer as a native-suite row; `bin/hb --load test/run.f`; two-generation build. Files: src/habu/habu2.f, bootstrap/cg/forth.fs where the seed mirrors the path, the test row. Claim: unassigned.

Reproduced on 387d46d0 (2026-10-23); reproducers ~/.cache/tmp/jerry-t0fix-cases/. c1-does-wide-nohead.f: tier 0 `1 2 7` / `1 2 3 7`, tier 1 `7 2 1` / `7 2 1 3`. Cause confirmed by reading: the clause check (checker.f:26473 CHECK-DOES-RUN) writes the one certificate buffer and the head check (checker.f:26118 CHECK!, from habu2.f:3160 C-CALL-CHECK-DEFINER) overwrites it, so LOWER-TXN:FREEZE (habu2.f:11406) freezes the head's; habu2.f:11582 reads the clause NEEDS only for TRUSTED:. Shares its fix with habu-lower-width-facts-2b584c6e and the TRUSTED: does> half of habu-compile-wide-values-624b62cf. Lane: jerry, worker in .jj-ws.noindex/jerry-t0fix.
Design (jerry-t0fix handoff, lead-approved 2026-10-23): the checker keeps the clause certificate across the head check (src/core/lower-cert-base.f double buffer, lower-cert:keep / lower-cert:kept; an engine-side copy would leak on the recoverable head reject); FREEZE copies the whole body and both certificates; pass 2 skips the clause signature parse at J-DOES and switches to the clause certificate there (source base += DOESB); EM-COMPILE-PUBLISH-TRUSTED gains a pass-2 guard; EM-DOES-WIDE-REFUSE, P2DOESW-MSG$ and LP2DOESW go; bootstrap/cg/forth.fs mirrors it. This extends the pass-2 replay that habu-read-each-def-f7ce769a milestone C deletes. Out of scope, unchecked: kernel-x64.f:4951 has pass-2 code of its own.
