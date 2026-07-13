---
title: Reject duplicate dot IDs across paths
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T13:47:03.012248+02:00"
---

Full context: .dots/habu-nominal-storage-raw-a3430ef2.md shadows .dots/habu-checker-seal-nominal-0b2eaece/habu-nominal-storage-raw-a3430ef2.md. dot show habu-nominal-storage-raw-a3430ef2 reports Ambiguous ID while tools/dot-dep-lint.f still reports zero findings, so parallel workers can claim or edit different files under one logical ID. Fix: extend the checked Habu dot dependency lint to index every .dots/**/*.md basename, reject duplicate IDs with both canonical paths even when one file lacks valid frontmatter, add focused nested/top-level and malformed-shadow fixtures, fold the stray evidence into the authoritative nested dot, and remove the shadow file. Acceptance: current collision fails red before cleanup; fixtures pin deterministic diagnostics; after cleanup dot show is unique and dot-dep-lint, host-lint, filemap-lint, and full native gate pass. Files: tools/dot-dep-lint-core.f, tools/dot-dep-lint-test.f, the two colliding dot records, LESSONS.md if a new lesson is learned.
