---
title: Fold stale-lint output buffers into lint text facility
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T10:47:44.000306+02:00"
---

Why: tools/stale-status-lint-core.f carries its own OUT-BUFFER!/ERR-BUFFER!/OUT$/ERR$/BUFFERS-OFF capture pair while tools/lint/text.f already provides the buffered-output facility (LINT-OUT-BUFFER! and siblings); the duplication was noted during the 2026-07-26 packaging lane and deliberately left out of that commit because folding changes the public test-capture seam the packaging froze. Behavior: migrate the stale-status lint core and its test to the shared tools/lint/text.f facility, delete the package-local duplicate words, and keep diagnostics byte-identical (prove with the same before/after byte-comparison discipline the packaging lane used: identical stdout, stderr, and exit codes across the clean, date-mismatch, and bad-today CLI paths). Owner: package STALE-STATUS-LINT in tools/stale-status-lint-core.f plus its test package. Dependencies: none; the packaging landed. Acceptance: no OUT-/ERR- capture word remains defined in the stale-status packages; the focused suite and the lint-tools slice green; typed-local and package diff lints accept the diff; byte-identity evidence recorded in the report.
