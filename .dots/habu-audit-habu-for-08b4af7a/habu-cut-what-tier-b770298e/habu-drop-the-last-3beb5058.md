---
title: Drop the last write-only record fields and dead files
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:08:07.668600+01:00"
---

Problem: the owner record's TRUST-DECL and LOCAL-WIDTH fields are filled and never read; TFL-VAR? has only a test caller; src/compiler/native/fetch-check.f is loaded and never called (the --checks dot restores checks from history); the primitive rows' R-BUILD reach and its build atom mark no row. Acceptance: each is deleted with its stores, rows and requires; the engine builds (through a stage if needed) and its link, engine and parity tests pass. Depends: none. Claim: unassigned.
