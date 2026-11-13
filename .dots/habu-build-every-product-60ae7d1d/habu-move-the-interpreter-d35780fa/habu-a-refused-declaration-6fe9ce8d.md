---
title: A refused declaration under --load prints a second uncaught-throw line
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:35:14.472558+02:00"
---

Problem: under --load, a refused declaration prints its diagnostic and then `hb: uncaught throw code N`, rc 70, while a refused colon body or a top-level E-UNDEFINED prints the diagnostic alone. Reported for STRUCTURE (7109), SUMTYPE (7107), TRUSTED: with a bad stored signature (7156) and cast:/linear: (7129); the cause is not known. The uncaught reporter (src/habu/habu2.f:2643-2685, LUNCAUGHT) writes that line for every code outside 1..255 and reads REFUSAL-ABI:CODE-CELL (layout.f) only to choose RC-REJECT, so the lead to test is which path keeps a refused colon body's throw from reaching the reporter while a declaration's reaches it. Reproducer: a file whose one line is a STRUCTURE declaration the checker refuses, run with bin/hb --load. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces.
Acceptance: the cause is reported at file:line; then a refused declaration of each of the four kinds, on a rebuilt hb, prints its diagnostic once, as a refused colon body does, and exits 70.
Depends: none.
Claim: unassigned.
