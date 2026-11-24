---
title: Divide basis sums over their live limbs
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.433558+01:00"
---

Problem: FBASIS:SCALE3 (src/lib/fmath-basis.f) takes 7.2 ms per call, 98% of an 11 s Maki weld edit (tim): every limb loop runs over all 408 limbs, RATIO-BITS re-shifts the divisor for each quotient bit, and scalar loop state sits in work cells read through bounds-checked SPAN:CELL@. Acceptance: SCALE3 works over each buffer's live limbs and divides with one shift per quotient bit; every result bit and throw is unchanged on a fixed 3000-case corpus. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fbasis.
