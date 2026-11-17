---
title: "Make FROUND cost its arithmetic"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T23:41:32.629284+01:00"
---

Problem: FMATH:FROUND (src/lib/fmath.f) costs about 27 ns per call on spark's X925 cores against FMAX's 7; Maki's mesh staging calls it 4.1M times per build. A third of it is the `f>=` and `f<=` it calls: src/lib/prelude.f:18-19 defines them as colon words that call `f>` or `f<` and `f=` and OR them (3.7 ns each, against 1.7 ns for the `f<` primitive). Acceptance: `f<=` and `f>=` are single-compare engine primitives on every target, false on a NaN operand, and FROUND's results are bit-identical (ROUND-EDGES, ROUND-VALUES). Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-fge.
