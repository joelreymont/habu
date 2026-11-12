---
title: "Add FMATH:ACOS"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T11:25:46.489901+02:00"
---

Problem: Maki's mesh levels (Maki dot 5fea0508, view-a) set each level's angular deflection as 2 acos(1 - 4^k (1 - cos 0.25)), and FMATH's only trig is PI, SIN and COS (src/lib/fmath.f:314-336). Acceptance: FMATH:ACOS ( r -- r ) answers acos in [0, pi] within 1 ulp of a correctly rounded value at sampled points, exact at -1, 0 and 1, and E-DOMAIN for a nonfinite input or one outside [-1, 1]; fmath-test covers it. Depends: none. Claim: agent=tim workspace=.jj-ws.noindex/tim-acos.
