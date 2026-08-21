---
title: "Lint diff: share hunk parser"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-15T07:29:26.401667+02:00"
closed-at: "2026-07-19T22:07:42.083830+02:00"
close-reason: "Landed at master 10f1f48e (commit 'Recover shared jj-diff parser; migrate lints'): tools/lint/diff.f is the single shared unified-diff event parser (package DIFF, typed form/event ENUMs, DIFF:LINE event API, fail-closed E-DIFF-SYNTAX in the new shared tools/lint/diff-error.f home), with a 495-line suite. Both production consumers migrated onto it - typed-local-diff-lint and kernel-perf-lint now dispatch on typed DIFF events via their own packages - with behavior preservation proven baseline-green-before / migrated-green-after, stricter exact-finding positives, and the spoofed-header-outside-hunk negatives the dot demanded. Recovered from the stranded recover-lint-diff lane and adapted; the M5 frame codec deliberately remains on the recovery dot habu-recover-framed-and-32309120, to be rebuilt on this parser rather than recovering its duplicate validator."
---

Full context: tools/typed-local-diff-lint-core.f already has TLD-IN-HUNK/TLD-PARSE-HUNK, while tools/kernel-perf-lint-core.f treats file-header-looking added lines outside @@ hunks as real headers; duplicating more parser state would violate the one-concern rule. Fix: add one checked tools/lint/diff.f unified-diff event parser with explicit file/hunk/add/context/delete events and fail-closed malformed input; migrate typed-local and kernel-perf scanners to consume it. Preserve typed-local behavior and add spoofed '++ b/path' outside-hunk negatives for both consumers. Acceptance: headers are recognized only in header state; content is emitted only inside a valid hunk; malformed/truncated hunks reject; formatting-only valid diffs preserve current findings. Files: tools/lint/diff.f, tools/lint/diff-test.f, tools/typed-local-diff-lint-core.f and test, tools/kernel-perf-lint-core.f and test, FILEMAP.md. Verify: both focused lint suites, typed-local self-diff, host/filemap/trust/dot gates. Claim: agent=diff-parser-fix workspace=.jj-ws/habu-lint-diff-recover.
Release 2026-07-19: claim agent workspace was destroyed in the .jj-ws loss incident (see LESSONS.md); lane returned to open for re-dispatch.

Claim: agent=diff-recover workspace=.jj-ws/habu-recover-framed-and-32309120 (serialized after the side-content slice in the same lane)
