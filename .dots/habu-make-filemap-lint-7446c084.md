---
title: Make filemap lint size-independent
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-17T19:54:03.924303+02:00\""
---

Full context: rebased integration head xnvvyynx grows FILEMAP.md to 132777 bytes, exceeding tools/filemap-lint.f FM-BUF-CAP=0x20000; filemap-lint exits 'lint: file exceeds buffer: FILEMAP.md' before checking paths. Cause: filemap-lint duplicates the shared dynamic LINT-SOURCE reader with a fixed 128KiB buffer. Fix: scan LINT-SOURCE:TEXT directly, remove the fixed buffer/cap, and add a >128KiB fixture proving derived path checks remain green. Acceptance: fixture, live filemap-lint, lint-tools slice, typed-local/host/dot lints, and full native integration gate green.
