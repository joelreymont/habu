---
title: "Tests: migrate engine diagnostics"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:18:50.043833+02:00"
blocks:
  - habu-tools-check-unified-fb3b67f6
---

Own engine, checker diagnostic, source-certification, and all-errors fixtures containing legacy declarations. Convert positives to unified syntax, keep removed syntax only in pinned rejection fixtures, and preserve exact expected/actual rows, source spans, package names, and exit codes.
