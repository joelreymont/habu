---
title: "typed-top: xt<effect> values"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T10:38:59.764294+02:00"
---

Sub-dot 4 of docs/typed-top-level.md sec 5 (landed 8cefda08). Files: src/core/checker.f ([''] retype from PE-N; execute/catch/run-in-stack/is/set-check/check@ operand retypes; top-row RSEXEC/RSCATCH reuse), test/xt-effect-test.f. Acceptance: ['] A execute certifies when the effect fits and rejects when not; tier-1 warning for ' FOO2 execute at underdepth; 0 0 catch warns (tier 1) with the tier-2 reject fixture pre-armed; underdepth-gate positives stay green. Depends: tracker sub-dot.
