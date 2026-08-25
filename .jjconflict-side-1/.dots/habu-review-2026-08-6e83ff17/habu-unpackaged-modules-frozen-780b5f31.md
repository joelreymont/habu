---
title: unpackaged modules frozen by the package lint
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.939463+02:00"
---

Problem: files with no package: lib/fs.f, fs-mutate.f, source.f, map.f, process-argv.f, float.f, lib/test/runner.f (GT-* globals), assert.f, snap.f, outcome.f, record.f, adt/option.f, adt/result.f, layout/box.f, type/extent-role.f, and 50 of 90 lib/ptx/*.f; lib/ptx/cg.f:105-111 and tile-v4a.f:452-459 record that they stay global because the package lint refuses any edit to a definition outside a package (E-PACKAGE-OWNERSHIP) - the lint that enforces packaging prevents packaging. lib/errors.f:198-206 opens package JR for three codes while E-JR siblings stay global. AGENTS.md: every module opens a real package. Acceptance: a lint mode (or a one-commit allowance keyed on 'the whole file gains a package in this diff') that accepts a packaging commit; the listed files packaged, callers qualified, no forwarding shims. Files: tools/package-diff-lint-core.f, the listed lib files. Verify: package-diff-lint exit 0; full gate. Depends: none. Ownership: packaging. Claim: unassigned.
