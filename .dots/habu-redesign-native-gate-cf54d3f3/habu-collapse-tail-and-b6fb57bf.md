---
title: Collapse tail and lint slice bottlenecks
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:33.490508+02:00\""
---

Problem: Mac hot test suite still spends ~7s in stdlib lint artifacts/lint tools and ~7-10s in stdlib tail slices. These are mostly host-source semantic checks and should run resident with shared setup. Fix: profile per-test spans, move pure lint/doc/tail semantics in-process, isolate only filesystem/artifact/process-contract sentinels, and ensure setup is per suite/group not per test. Acceptance: each tail/lint slice under 5s on Mac hot profile and report names every test/group with timing.

Progress: macos-arm64-12x2 hot proof on 2026-06-30 reports split resident groups
for tool-lint, lint-libs, lint-artifacts/fast, and tail cohorts. Keep open:
tail-pure is 7532ms, lint-libs/core is 7502ms, lint-tools is 7663ms, and
all-strict diagnostics is 7906ms.
