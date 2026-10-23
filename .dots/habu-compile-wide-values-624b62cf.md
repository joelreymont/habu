---
title: "Compile wide values in tier-0 TRUSTED: bodies"
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:43.037641+03:00"
---

Problem: at tier 0 a wide value in a TRUSTED: body or a TRUSTED: does> clause compiles as one cell; tier 1 is correct (2026-10-10 probes trp, trd). Cause recorded 2026-10-10 (carl), unverified since: the head and clause checks write one certificate buffer and pass 2 replays only the head's (src/habu/habu2.f C-CALL-CHECK-DOES, LOWER-TXN:FREEZE, EM-P2-TRIGGER). The original probes are gone: reproduce first, at tier 0 (`0 set-tier`) and tier 1 (`1 set-tier`) on ARM64. Tier 1 is the reference.
Acceptance: both shapes print tier 1's output at tier 0. Verify: rebuild bin/hb per docs/gate.md; the reproducer as a native-suite row; `bin/hb --load test/run.f`; two-generation build. Files: src/habu/habu2.f, bootstrap/cg/forth.fs where the seed mirrors the path, the test row. Claim: unassigned.

Reproduced on 387d46d0 (2026-10-23); reproducers ~/.cache/tmp/jerry-t0fix-cases/. Two causes. Plain body (c2a-trusted-body.f: tier 0 `1 2 7`, tier 1 `7 2 1`): the ttrusted arm of EM-COMPILE-PUBLISH-TRUSTED (habu2.f:11589-11594) publishes with no scan, certificate or pass 2, where tier 1 scans under the trusted regime (compiler.f CHECK-PARENT); lane: jerry, worker in .jj-ws.noindex/jerry-t0trusted. TRUSTED: does> clause (c2b-trusted-does.f): tier 0 refuses rc 75 (EM-DOES-WIDE-REFUSE, habu2.f:11581-11587) rather than compiling one cell; fixed with habu-compile-wide-values-cb22400a in .jj-ws.noindex/jerry-t0fix.
