---
title: Inline primitives in tier 0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:19.000537+03:00"
closed-at: "2026-10-27T03:40:59.708649+02:00"
close-reason: all three steps landed and measured; 5% bar not met
---

Experiment, approved by Joel 2026-10-25 (rulings in habu-decide-whether-one-358fc93b): tier 0 copies a primitive's body from the primitive table instead of calling it, then (separate steps) keeps stack values in registers and ?do counters in registers; measure each step on the kernels, the real workloads and tools/compile-floor.f. If tier 0 runs real workloads within 5% of tier 1 at about tier 0's compile cost, tier 1 can go. Depends: the census and timing children. Claim: jerry.

Step 1 done (commit "Inline primitive bodies in tier 0"; test/compiler/tier0-inline-prims.f pins it): tier 0 copies primitive bodies, test/run.f 501/501, two-generation build gen 5 = gen 4. It does not move tier 0 toward tier 1. User-CPU medians of 3 runs on ARM64, base 0773ab3f vs step 1 (run = whole program - load only; host load 3-12, wall times noisier):

| workload | t0 load | t0 run before | t0 run after | t1 run |
|---|---|---|---|---|
| tools/tier-bench.f | 0.04 | 0.50 | 0.49 | 0.21 |
| zz daily | 0.12 | 3.02 | 2.92 | 1.02 |
| dots ls (x300) | 0.25 | 1.62 | 1.59 | 1.05 |
| dots tree (x300) | 0.25 | 3.02 | 3.15 | 2.20 |
| tender c2-office-scale | 1.84 / 1.73 | 66.8 | 64.5 | 38.6 |

Tier 0 compile cost is unchanged (load times equal; tools/compile-floor.f trivial-t0 42-44 / least 32 ms both, against trivial-t1 ~590 / least ~560 ms). Tier 0 still runs 1.5-2.4x slower than tier 1, so step 1 alone is far from the 5% bar; the call overhead it removes is not where tier 0 loses. Next: step 2 (stack values in registers).

Rulings (Joel 2026-10-26, after the Fable review of the outside tier 0 plan): order is fixes first (habu-fix-the-tier-4d7b9733, habu-check-tier-bench-57f504d5), then step 2 (habu-keep-tier-0-bfdcdd19), compare/branch fusion (habu-fuse-tier-0-c829c4e9), register residency across joins as T0-04 minus copy-on-write aliases (habu-keep-tier-0-730a5c5d, with habu-widen-the-tier-8c29a00c; aliases deferred in habu-copy-on-write-a885df73), and step 3 last (habu-keep-the-innermost-636aeac0); measure after each. Retirement prerequisites are dotted as children blocked on step 3, to do only if the ruling retires tier 1.
