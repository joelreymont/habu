---
title: Remove backup files
status: open
priority: 2
issue-type: task
created-at: "2026-02-02T22:28:26.316229+01:00"
blocks:
  - habu-update-parity-doc-7d82de5d
---

Context: src/**/*.bak*, stdlib.habu.bak*; cause: stale backups w/ forbidden patterns; fix: trash .bak* files; deps: habu-update-parity-doc-7d82de5d; verification: rg --files -g '*.bak*' returns none
