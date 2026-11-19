---
title: "Check a call's nested quotation input in linear time"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:57:57.680549+01:00"
---

Problem: checking a call whose input quotation nests k deep (test/split-str-deep.f QUOT-CALL) grows faster than linearly: master's engine takes 0.24 s at k=400, 0.61 s at 800, 3.68 s at 1600 and 7.72 s at 2047, and the loop lane (skmulorv) 0.26, 1.30 and 10.83 s up to 1600. Inferred, not measured: each nested unify level's HOLD-PUSH (src/core/checker.f, QUOT-PROV-INST) renews the whole remaining depth through RENEW*. Acceptance: the times per doubling of k stay near 2x from 400 to 2047, with the cause named. Depends: none. Claim: unassigned.
