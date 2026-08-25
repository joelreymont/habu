---
title: Grow the recorded-body table with the program
status: open
priority: 4
issue-type: task
created-at: "2026-08-03T16:09:46.128814+02:00"
---

src/compiler/native/inline.f holds 64 recorded bodies in a fixed table (ROWS-MAX), 16 tokens and 24 spelling bytes each, because it runs while the engine is compiling and has nowhere to allocate from - the same reason src/compiler/native/publish.f's log and src/compiler/native/clobber.f's table are fixed. ROOM-CK refuses a migration by name (E-NINL-CAP) once it is full rather than silently forgetting a body, so a whole-system migration of more than 64 small routines stops. Dot habu-grow-the-republication-52ef5df0 carries the same problem for the publication log and the two should land together: whatever gives that log room to grow with the program gives this table room too. Owner: NINL.
