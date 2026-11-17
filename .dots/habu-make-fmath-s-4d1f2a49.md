---
title: "Make FMATH's FMIN, FMAX and FROUND cost their arithmetic"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T23:22:09.240956+01:00"
---

Problem: FMIN negates both operands and calls FMAX, which tests each for NaN through locals before comparing (src/lib/fmath.f:283-296), and FROUND's ROUND-DOMAIN computes its limit ($4000000000000000 s>f 2.0 f*) on every call (:102-105). Maki's mesh staging calls FMIN and FMAX about 8.3M times and FROUND 4.1M times per crossmember build; FMIN and FMAX alone took about 110 ms of its 1.37 s (Maki hunt meshstage, 2026-11-17, /tmp/tim-meshstage). Acceptance: every result bit-identical to today's (maximumNumber and minimumNumber: NaN operands, +0 above -0 in either order; FROUND's domain checks), ns per call before and after on spark. Depends: none. Claim: unassigned.
