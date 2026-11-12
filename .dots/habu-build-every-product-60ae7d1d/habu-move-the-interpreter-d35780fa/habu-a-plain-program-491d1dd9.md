---
title: "A plain program's refusal prints no file or line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:35:50.344459+02:00"
---

Problem: a refusal in a plain program, run as `bin/hb file.f` (the root stream, with no include frame), was reported to print `E-UNDEFINED: nosuchword` with no file or line, while the same refusal under --load names the file. The cause is not known. Reproducer: a file holding the one line `nosuchword`, run as bin/hb file.f. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces.
Acceptance: the cause is reported at file:line; then the reproducer, run both ways on a rebuilt hb, prints the same located line.
Depends: none.
Claim: unassigned.
