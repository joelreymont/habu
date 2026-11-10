---
title: A refused declaration under --load prints a second uncaught-throw line
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:35:14.472558+02:00"
---

Problem: under --load, a refused declaration prints its diagnostic and then `hb: uncaught throw code N`, rc 70, while a refused colon body or a top-level E-UNDEFINED prints the diagnostic alone. Reported for STRUCTURE (7109), SUMTYPE (7107), TRUSTED: with a bad stored signature (7156) and cast:/linear: (7129); the probe files are gone and the cause is not known. A lead to test: the uncaught reporter (src/habu/habu2.f:12596-12605, LUNCAUGHT) adds no line for the refusal the checker last rendered (layout.f REFUSAL-ABI:CODE-CELL), so the declaration's throw may carry a code that cell does not hold. Reproducer: a file whose one line is a STRUCTURE declaration the checker refuses, run with bin/hb --load. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces.
Acceptance: the cause is reported at file:line; then a refused declaration prints its diagnostic once, as a refused colon body does, with the same rc 70.
Files: per the cause.
Verify: the reproducer for each of the four declaration kinds on a rebuilt hb prints one diagnostic and exits 70.
Depends: none.
Ownership: unassigned. Claim: unassigned.
