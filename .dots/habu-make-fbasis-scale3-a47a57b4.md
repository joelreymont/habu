---
title: "Make FBASIS:SCALE3 cost what its numbers need"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T18:44:13.576233+01:00"
---

Problem: FBASIS:SCALE3 takes about 7.1 ms per call (tim's reproducer /tmp/tim-scale3), so a Maki weld span edit of 1,614 calls takes 11 s. RATIO-BITS (src/lib/fmath-basis.f:201) runs 53 full-width 408-limb passes, each shifting from scratch, and every limb and counter goes through bounds-checked span access. Acceptance: 1,000 calls of the reproducer in at most 0.15 s, with results bit-identical to the old code. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-scale3.
