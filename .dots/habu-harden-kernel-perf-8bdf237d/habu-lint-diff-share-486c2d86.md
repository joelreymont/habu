---
title: "Lint diff: share hunk parser"
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T07:29:26.401667+02:00"
---

Full context: tools/typed-local-diff-lint-core.f already has TLD-IN-HUNK/TLD-PARSE-HUNK, while tools/kernel-perf-lint-core.f treats file-header-looking added lines outside @@ hunks as real headers; duplicating more parser state would violate the one-concern rule. Fix: add one checked tools/lint/diff.f unified-diff event parser with explicit file/hunk/add/context/delete events and fail-closed malformed input; migrate typed-local and kernel-perf scanners to consume it. Preserve typed-local behavior and add spoofed '++ b/path' outside-hunk negatives for both consumers. Acceptance: headers are recognized only in header state; content is emitted only inside a valid hunk; malformed/truncated hunks reject; formatting-only valid diffs preserve current findings. Files: tools/lint/diff.f, tools/lint/diff-test.f, tools/typed-local-diff-lint-core.f and test, tools/kernel-perf-lint-core.f and test, FILEMAP.md. Verify: both focused lint suites, typed-local self-diff, host/filemap/trust/dot gates.
