---
title: "Lint: gate sole type surface"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:49.746782+02:00"
blocks:
  - habu-lint-detect-removed-b53f1090
---

Integrate the removed-token lint into native lint-tools/full gate, FILEMAP, STATUS, and focused positive/negative fixtures. Prove only STRUCTURE/;STRUCTURE and ENUM/;ENUM declare public composite types, runtime lookup cannot find removed words, and allowlist drift fails.
