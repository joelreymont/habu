---
title: "Reject a negative count in VEC:EXTEND"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T20:55:26.342245+01:00"
---

Problem: src/lib/vector.f EXTEND (:139-143) with a negative n whose sum with COUNT stays at or above 0 passes ENSURE's early exit (:108) and answers an empty span with COUNT unchanged, while RESIZE (:118) and APPEND (:173) reject a negative with E-VEC-CAPACITY. Acceptance: EXTEND of a negative n is E-VEC-CAPACITY with the vector unchanged, as RESIZE's is, and vector-test pins it. Depends: none. Claim: unassigned.
