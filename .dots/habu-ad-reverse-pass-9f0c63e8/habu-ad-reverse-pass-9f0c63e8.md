---
title: "AD reverse pass: straight-line, control-flow reject"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:00:43.545813+02:00"
---

Decomposes ad-reverse (habu-ptx-ad-reverse-26aebee3). Steps 1-4 of autograd.md The-reverse-pass: linearise W to its typed IR word list, reverse-and-substitute VJP[wn]..VJP[w1], thread cotangents (fan-out DUP becomes sum +.), supply saved values. v0 is STRAIGHT-LINE ONLY: any forward containing IF/loop/RECURSE entering the reverse pass is REJECTED fail-closed with a diagnostic (list-reversal reverses dataflow not control flow - that is exactly why PyTorch keeps a tape). Control-flow reversal is a separate dotted capability.
- Files: new src/arch/ptx/ad.f (the pass over the typed IR from M2/the checker).
- Verify: a straight-line softmax forward reverses; a forward with an IF is REJECTED with the named diagnostic (negative regression). 
- Dep: BROADCAST is landed; remaining BLOCK-MAX-select primitives + the typed IR (M2).

2026-06-30 local checkpoint: added a named straight-line-boundary reject in `lib/ptx/ad.f`; control-flow tokens now throw `E-PTX-AD-CONTROL` before VJP expansion with case-insensitive matching, and `lib/ptx/ad-test.f` covers lowercase `LOAD if STORE then`. Remaining parent work is deeper typed-IR/rewrite integration and cotangent/lowering proof; no zed/device validation was attempted.
