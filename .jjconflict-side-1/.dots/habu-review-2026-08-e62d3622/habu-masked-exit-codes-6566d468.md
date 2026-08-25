---
title: "masked exit codes: seven negative throws are multiples of 256"
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.186848+02:00"
---

Problem: tools/build-fixpoint.f:1969-1975 says the engine exits with the throw code masked to 8 bits and no diagnostic; habu2.f:617,6030,8153-8158 now has an uncaught-throw reporter with an out-of-range branch (9 255 CMPI, C-GT) - whether negative codes still exit masked is unverified; seven codes are multiples of 256: E-JUDGE-SRC-ROW -8704 (lib/errors.f:1243, thrown at tools/judge/src.f:122,369,435,443 with no catch on the judge CLI path), E-JRPP-CHILD -6400, E-IR-ARENA-CEIL -6656, E-A64RAV-SHARE -8448, E-RB-COLS -5120, E-ACTION-ID -5376, E-JRPP-FIRST. If masking applies they exit 0. Acceptance: measured on the engine; if masked, the uncaught path maps every code to a nonzero rc with the name on fd 2 and a test throws each of the seven; the stale comment corrected. Files: src/habu/habu2.f, tools/build-fixpoint.f, tools/judge.f. Verify: the test. Depends: engine on this host. Ownership: engine exits. Claim: unassigned.
