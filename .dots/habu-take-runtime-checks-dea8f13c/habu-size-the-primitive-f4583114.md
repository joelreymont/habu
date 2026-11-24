---
title: Size the primitive table by its rows
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T15:50:51.630991+01:00"
---

Problem: src/habu/prims.f ROWS, NAMES and CODES are fixed create tables (ROW-CAP, NAME-CAP, CODE-CAP) with a die past each. hb's image holds the table (src/habu/boot-files.f:57 loads prims.f in the engine prefix; src/core/checker.f NAV-PRIM-ROW walks it at run time), so storage a build does not save (DYNAMIC-BUFFER) cannot hold it. Acceptance: the table's storage is sized by the rows prims.f declares, with no capacity constant or cap die, and is saved in the image; engine builds converge. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-prims.
