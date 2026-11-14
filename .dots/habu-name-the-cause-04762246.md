---
title: Name the cause of a failed spawn
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T12:53:47.739841+02:00"
---

Problem: an uncaught refused spawn reports only "hb: uncaught throw code -2500" (the engine's generic reporter, src/habu/habu2.f:2340-2382), so neither the program path nor the errno reaches the reader. The errno itself is kept: PROCESS:SPAWN stores it in the task-local PROCESS:ERRNO (src/lib/spawn.f:251, :356), every RAW spawn goes through it, and FS:ERRNO$ (src/lib/fs.f:133) gives an errno's host text. Reproducer from Maki: its test/drawing-entry.f ran a missing native fixture through CMD:RUN-RC (src/lib/process-command.f:540) and died with an uncaught E-PROC-SPAWN that read as a resource failure; only rebuilding the fixture showed it was ENOENT. Where the path and errno text are written is a choice nobody has made: CMD's run words could write a line to stderr before the throw, as CMD:RUN already writes a failed run's stderr (src/lib/process-command.f:555-560), which callers that catch the throw would also see; or the uncaught report could carry the cause. Regressions are missing too: ENOENT is checked only through PROCESS:SPAWN (src/lib/spawn-test.f:331-338) and SPAWN-RAW on macOS (src/lib/process-test.f:155-159), and no test checks EACCES for a non-executable file.
Acceptance: Joel's choice of where a refused spawn's program path and errno text are written, recorded here; then an uncaught E-PROC-SPAWN from CMD's run words shows the path and the errno text, with regressions for a missing program (ENOENT) and a non-executable file (EACCES) through PROCESS:SPAWN-IO and CMD's run words.
Files: src/lib/process-command.f, src/lib/process.f, src/lib/spawn-test.f, src/lib/process-test.f, src/lib/process-command-test.f.
Verify: hb --load src/lib/spawn-test.f, src/lib/process-test.f and src/lib/process-command-test.f each rc 0; a program whose CMD:RUN-RC names a missing path dies with the path and "No such file or directory" on stderr.
Depends: none.
Ownership: unassigned. Claim: unassigned.
