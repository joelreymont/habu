---
title: Read fetch facts live in T0 instead of copying them
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T22:34:21.291482+02:00"
---

Problem: T0 (tier 0, the compiler that replaces the JIT) reads each typed fetch's facts from a copy: src/habu/t0.f:1196-1198 VALIDATE calls NFETCH:AT, which reads rows NFETCH:CAPTURE (src/compiler/native/fetch.f:162) copied out of the checker's certificate after the check. T0's SCAN reaches that copy through tier 1's definition code (t0.f:1846-1851 CHECK-UNIT calls src/compiler/native/definition.f:151-153 CHECK-SOURCE, :185 CHECK-DOES-HEAD and :196 CHECK-DOES-CLAUSE, each ending in NFETCH:CAPTURE). The copy costs load time and keeps T0 on a tier 1 dependency. Widths and binds already read live (CHECKER-OWNER:WIDTH-AT); this is the rest of that move, the lane once called S2B-2, step 1 of habu-t0-performance-steps-7c732798. Joel's rules for every T0 performance step: one technique per change; measured before and after on the same corpus with instruction counts (interleaved rounds, medians, output hashes; the script once at /tmp/jerry-meas/im.sh no longer exists); the bar written in the brief before work starts; outputs identical; each lands alone; no step without a number.
Acceptance: T0 reads fetch facts from the checker live and no word T0's SCAN reaches calls NFETCH:CAPTURE; tier 1 keeps NFETCH; compiled output is byte-identical; the load's instruction count before and after on one corpus meets the bar written in the brief.
Files: src/habu/t0.f, src/compiler/native/definition.f, src/compiler/native/fetch.f, src/compiler/native/checker-owner.f.
Verify: rg shows no NFETCH:CAPTURE reached from t0.f's SCAN; the suite passes; output hashes and instruction counts before and after on the same corpus.
Depends: habu-delete-the-jit-d9fdaf67.
Ownership: unassigned. Claim: unassigned.
