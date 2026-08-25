---
title: Fix the wrong-token scope diagnostic
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T22:13:34.874685+02:00"
---

Checker false positive: a package-scope `variable U` makes a LATER definition's `a TRY-SRC !` fail with 'expected: a ptr a actual: n n' pointing at the wrong token — minimal repro: rename TRY-U to U in an otherwise-identical file and green flips red. Found by the immsel calibration lane 2026-08-07. Diagnose whether the collision is real (single-letter global shadowing something) and mis-reported, or spurious; either way the diagnostic must name the actual offender.
