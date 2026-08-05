---
title: Fix the result-cache closure scanner
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:01:50.381633+02:00"
---

test/run.f red phase stdlib/lint-artifacts/fast: 'result-cache closure: tools/checked-boundary-lint-core.f -> missing tools/hook-sites.f' — but tools/hook-sites.f exists in the tree, so the closure scanner itself has a real bug (wrong root, stale cache key, or path normalization). Reproduced identically on both parents of merge cd7bf8eb, so it predates the codegen merge. Find the scanner's owning source, build the minimal reproducer, fix the scanner, and keep a regression test with a fixture whose require closure includes the failing edge.
