---
title: Open any number of usings at once
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T13:49:38.628661+01:00"
blocks:
  - habu-move-the-interpreter-d35780fa
---

Problem: src/habu/layout.f:1127 USE-MAX (16) sizes the USE band in DATA, the checker's mirror CK-USE-NAMES (src/core/checker.f:1858) and the REPL's REPL-WIDS (src/habu/interpreter.f:3162); PKG-USING (interpreter.f:1182-1184) rejects a 17th open using with E-USING-OVERFLOW, and checker.f CHECKER-USING (:13442) dies INTERNAL at the same depth. habu2.f:901 and kernel-x64.f:4883 walk the band in machine code. A limit Habu sets on itself. Acceptance: usings nest to any depth in the REPL, a file and the checker; a test pins it and fails first. Depends: habu-move-the-interpreter-d35780fa (habu2.f is replaced machinery). Claim: unassigned.
