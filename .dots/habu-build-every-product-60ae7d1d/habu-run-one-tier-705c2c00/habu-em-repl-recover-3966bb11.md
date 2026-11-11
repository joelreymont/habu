---
title: EM-REPL-RECOVER restores cells only LREAD writes
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.223839+02:00"
---

Problem: found by the JIT reach census (jit-census-notes.md item 4), inferred, not run: the one path left into habu2.f LMAIN is an uncaught throw outside REPL-LINE's catch on a tty REPL (LRREC -> LREAD -> LMAIN), where the constant, create, variable and defer rows (habu2.f :10751-10768) emit through JIT helpers with no tier check; EM-REPL-RECOVER restores cells only LREAD writes. Acceptance: a tty REPL's uncaught throw recovers into the Habu loop, and LMAIN has no caller, so the JIT deletion can remove it. Files: src/habu/habu2.f, src/habu/repl.f. Verify: a tty test that throws outside REPL-LINE's catch and then defines a constant. Depends: none; blocks the LMAIN cut of habu-delete-the-jit-d9fdaf67. Ownership: unassigned. Claim: unassigned.
