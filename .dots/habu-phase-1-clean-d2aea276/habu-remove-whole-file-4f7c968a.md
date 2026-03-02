---
title: Remove whole-file load caps
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.122575+02:00"
blocks:
  - habu-harden-trusted-root-0a2b245c
---

Problem: load has a 1 MiB whole-file cap and is not scalable for real Maxima sources. Acceptance: load streams or otherwise handles large files such as share/draw/wbd.lisp. Files: src/interp/repl.zig:2096. Verify: load of large upstream files succeeds without cap-specific failure. Blockers: habu-harden-trusted-root-0a2b245c.
