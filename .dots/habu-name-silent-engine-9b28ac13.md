---
title: Name silent engine capacity exits
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T00:08:28.282713+02:00"
---

Static invariant: every fatal engine exit must identify itself on stderr before dying; the boundary is the emitted fail paths in src/habu/habu2.f. Evidence: the boot source-prefix copy loops exit bare rc 74 with NO message when the concatenated prefix reaches IBUFSZ (SRC-SFAIL habu2.f:747, SRC-BFAIL habu2.f:796, plus the read-error 74s at habu2.f:343/353/1941 and habu1.f:2185 bfail). Item 12 slice-3a lost an hour to this: install --force surfaced only 'E-BUILD-STATUS: refresh child failed' and the child printed nothing (LESSONS entry 2026-07-06). Fix: give each raw 'MOVZ 74 + NR-EXIT-GROUP' site a short static message written to fd 2 before the exit (pattern exists: LOPENERR prints 'hb: cannot open'), same for the Gforth mirror bootstrap/cg/forth.fs; add a fixture that forces the IBUFSZ overflow in a child (tiny IBUFSZ test build or seeded oversized prefix) and asserts the message names the buffer.
