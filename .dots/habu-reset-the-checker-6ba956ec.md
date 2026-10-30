---
title: "Reset the checker's letter maps at each body scan"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T21:55:04.696725+02:00"
---

Finding (SCRATCH audit, carl-fscr/.out/l11/scratch-handoff.md): NMAP and ROWMAP (src/core/checker.f) rest at -1 (UNBOUND), and VAR-OF and RVAR-OF (through LOCAL-TYPE) read them with no reset since the last parse, so in a body with no signature a local annotation reads the previous definition's letter map. From reading the code; not reproduced. Acceptance: a rejected or admitted program showing the leak (a definition without a signature whose local annotation uses a letter the previous definition bound), then both maps reset at body-scan start; the program's verdict is the one a fresh checker gives. Files: src/core/checker.f. Verify: the program through the real load path, before/after. Depends: none. Ownership: unassigned. Claim: unassigned.
