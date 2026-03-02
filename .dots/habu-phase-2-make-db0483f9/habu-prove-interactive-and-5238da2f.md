---
title: Prove interactive and scripted Maxima
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.176224+02:00"
blocks:
  - habu-fix-mforma-blocker-bd27ff36
---

Problem: clean load must survive repeated interactive and scripted evaluation. Acceptance: representative simplify, factor, solve, and integrate runs are stable across repeated sessions. Files: shared manifest/loader entrypoints and script runners. Verify: repeated script and REPL smoke with stable package/context state. Blockers: habu-fix-mforma-blocker-bd27ff36.
