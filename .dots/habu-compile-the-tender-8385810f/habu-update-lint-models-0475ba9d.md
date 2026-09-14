---
title: Update lint models for shared address storage and guarded stage0 atomics
status: active
priority: 2
issue-type: task
created-at: "2026-09-14T03:42:44.487887+03:00"
---

Native M full gate: clobber-lint and its fixture fail because MARK-HEADER reloads x4/x16 but the emitter scan ignores the helper; friend-arena-absence still declares atomic! and atomic-cas absent although their stage0 implementations guard their exact eight-byte destination. Model the real emitted register writes and pin the guarded atomic bodies. Regressions must continue rejecting a missing post-SYS reload and a removed/misdirected guard. Acceptance: both actual source lints and their negative controls pass on M; no production or absence checks weakened.

Focused native M source-path checks all pass: clobber 0.664 s, clobber fixtures 0.665 s, seal-absence 0.565 s. Evidence `/tmp/cedar-M-lint-fix/results.json`. The new emitter fixture has both a post-SYS MARK-HEADER reload and a missing-reload control; the latter remains a finding. Stage0 tests pin the two actual eight-byte atomic guard sequences and reject missing/wrong-destination guards. Independent review pending.
