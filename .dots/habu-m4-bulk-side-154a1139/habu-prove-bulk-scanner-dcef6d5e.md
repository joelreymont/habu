---
title: Prove bulk-scanner milestone
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:09:24.594488+02:00"
blocks:
  - habu-tools-bulk-diff-f36d0508
---

On the exact M3-based tree, run real jj external-diff fixtures for absent, empty, text, binary, symlink, gitlink, duplicate rows, LF/CR/adversarial path bytes, corruption/truncation/order/path escape, and N>=2 constant child count. Match and propagate every checked FS stream outcome, register focused gates/manifests/FILEMAP, build the real scanner through hb-build, independently review, run full gates, then fast-forward green master and close M4.
