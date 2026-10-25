---
title: Name the error and the file when a loaded file fails
status: open
priority: 1
issue-type: task
created-at: "2026-10-25T08:48:45.500830+01:00"
---

A --load'ed file that leaves cells ('1 2') or uses a retired form ('NEWTYPE length 0') ends with only 'hb: uncaught throw code -3804', rc 67: no error name, no file, no line (master 3a5fe328). An undefined word prints 'E-UNDEFINED: nosuchword' with no location either. The compile-die tail already prints ' at <path>:<line>' from SRCLOC:PATH-CELL (src/habu/outer.f AT-SOURCE, src/habu/habu2.f EM-COMPILE-DIE), but INCLUDE-EVALUATE (src/core/include.f) rethrows a file's failure unnamed and THROW-REPORT (src/habu/main.f) prints the bare code. Acceptance: a load failure names the error (registered name from src/lib/errors.f) and the innermost file, with the line where one applies (residue is a whole-file property: name the file and the cell count). Found migrating etch to current Habu, which stops at the first such failure with no location among 155 files. Scope (design 2026-10-25): this dot covers residue (file + cell count, at the innermost file) and E-UNDEFINED (file:line, both loops); a caught refusal prints its one line and keeps its code, like every existing refusal. The other unlocated refusal sites and bare library throws moved to habu-locate-the-remaining-bf91377e. Brief: /Users/joel/.cache/tmp/jerry-lead.K6tX/diag-brief.md. Claim: jerry.
