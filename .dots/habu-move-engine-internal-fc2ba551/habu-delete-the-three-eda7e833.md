---
title: Delete the three repl-term.f files
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T17:35:19.919220+01:00"
closed-at: "2026-11-18T21:56:57.826365+01:00"
blocks:
  - habu-let-a-stripped-d305e42c
---

Problem: src/os/linux/repl-term.f, src/os/macos/repl-term.f and src/os/linux-x86-64/repl-term.f stay only because the hb installed before dot d305e42c builds gen 1 with its carried RUNTIME-FILES, which loads them; nothing in the tree loads them once d305e42c lands. Acceptance: the three files are gone and a gen 1-3 build from the installed hb passes. Depends: d305e42c landed and installed. Claim: unassigned.
