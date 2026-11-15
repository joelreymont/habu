---
title: "Take back a scan's parse-imm rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:24:15.038814+01:00"
---

Problem: VERIFY:SOURCE-BUF-THEN-IN-SCOPE at top level leaves the scanned source's parse-imm row in the checker's table after it closes (probe /tmp/carl-t8/p3.f: rows 0, 1 in scope, 1 after): RUN-IN-SCOPE (src/habu/verify-source.f:3892) opens the owner's window but no checker scope, though its comment says the close restores the caller's scope. The table holds PIMM-CAP 16 rows (src/core/checker.f:19740) and dies 'parse-imm table full' at 19814, a limit on a program's own declarations. Acceptance: a scan takes back every row it made; a program may declare any number of parsing immediates. Depends: none. Claim: unassigned.
