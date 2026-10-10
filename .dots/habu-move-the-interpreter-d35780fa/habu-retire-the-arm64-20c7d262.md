---
title: Retire the ARM64 tier-0 JIT
status: open
priority: 1
issue-type: task
created-at: "2026-10-10T02:02:07.956141+03:00"
blocks:
  - habu-move-whole-values-6bcab743
  - habu-compile-records-over-9e5f9ea6
  - habu-compile-an-empty-2d88de28
  - habu-refuse-an-unsigned-ea48c18b
  - habu-refuse-a-second-382f76b5
  - habu-inventory-the-gate-9d02ff35
---

Problem: docs/compilation.md decision 7 (Joel, 2026-10-08): ARM64's tier-0 JIT (src/habu/jit.f and the habu2.f compile path with its pass-2 replay) is not kept in the product; the Habu loop compiles every definition through the platform codegen. It is still bin/hb's default tier, and it miscompiles shapes tier 1 compiles correctly (bin/hb, measured): a wide value in a does> clause compiles as one cell when the head has no width facts (s-does-tr2 prints `3 7 2 1`, tier 1 `3 2 1 7`); a TRUSTED: body or TRUSTED: does> clause with a wide value compiles as one cell (trp, trd); a does> definer with width facts in its head is refused rc 75 `does>-split cannot lower layout width facts` (d5). Cause: the clause and head checks write one certificate buffer and pass 2 replays only the head's (src/habu/habu2.f C-CALL-CHECK-DOES, LOWER-TXN:FREEZE, EM-P2-TRIGGER; analysis in ~/.cache/tmp/carl-wide/handoff.md). Tier 0 gets no new machinery for these. Probes: ~/.cache/tmp/carl-wide/probes/ (d5, s-does-tr2 with s-base, trd, trp, trh, and their -t1 tier-1 twins).
Acceptance: bin/hb compiles every definition through tier 1 on ARM64; jit.f, the tier-0 compile path and pass 2 go, and `set-tier 0` is refused as on x86. d5 prints 19 20 21, s-does-tr2 `3 2 1 7`, trd and trp their tier-1 output, all in the native suite. The gate's tier-0 rows are retired or rewritten per habu-inventory-the-gate-9d02ff35.
Files: src/habu/jit.f, src/habu/habu2.f (the tier-0 compile path and pass 2), src/habu/prims.f and kernel bodies of set-tier, the gate rows habu-inventory-the-gate-9d02ff35 lists, bootstrap/cg/forth.fs (the seed mirror's tier-0 path), docs naming tier 0.
Verify: rebuild bin/hb per docs/gate.md; `bin/hb --load test/run.f`; two-generation build converges; the periodic no-binary check (docs/bootstrap.md).
Depends: habu-inventory-the-gate-9d02ff35; the native tier-1 dots habu-move-whole-values-6bcab743, habu-compile-records-over-9e5f9ea6, habu-compile-an-empty-2d88de28, habu-refuse-an-unsigned-ea48c18b and habu-refuse-a-second-382f76b5 (tier 1 must compile what the language admits and refuse what it refuses). Ownership: the files above. Worker: worker-max, after decomposition. Claim: unassigned.
