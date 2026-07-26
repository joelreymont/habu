---
title: Migrate raw vector callers to typed API
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T23:39:40.862768+02:00"
---

Leaf 1 of the vector-surface retirement. Measured (exact-token sweep, not -w which counts E-VEC-BOUNDS as a caller): the raw VEC-* surface has 61 external production references in 5 files - tools/dot-dep-lint-core.f (35), tools/lint/source-lex.f (10), tools/lint/intern.f (8), maki/sched-key.f (6), maki/sched-key-test.f (2) - plus 149 in lib/vector-test.f which retire with the surface in leaf 2. Migrate the five files onto the EXISTING typed package VEC public API (INIT, CLEAR, DISPOSE, LEN@, CAP@, RESIZE, ENSURE, PUSH, EACH, @, !): this is a semantic conversion where call sites adopt the typed word's roles (raw counts become CAD-NUM roles per the API's signatures), site by site, never a textual rename. Forbidden: any new public word on package VEC; any RAW-* bridge surface (rejected by ruling - it would permanently publish a surface lib/vector.f's own comment schedules for retirement); touching lib/vector.f itself (leaf 2 owns it). Acceptance: the five files consume only VEC: typed words (exact-token sweep zero raw references outside lib/vector.f and lib/vector-test.f); dot-dep-lint suite, both lint suites, sched-key suites green; both diff lints per commit. May split into two commits at subsystem seams (tools vs sched-key) if each passes its gates independently.
