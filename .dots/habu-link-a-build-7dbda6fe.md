---
title: Link a build whose caught evaluate allotted nothing
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T19:12:59.669971+01:00"
---

Problem: a build-time `: PROBE ( -- n ) [: s" 12345 throw" evaluate-closed ;] catch ; PROBE drop` (/tmp/jerry-acg/c2.f) makes tools/hb-build.f refuse the program with "it gave back data space (a caught failure or a negative allot)" (src/habu/linker.f END-CK :5349) though the text allotted nothing: DATA-SPACE:REWIND (src/core/rollback-cells.f:67-74) raises DPHW-CELL to the DP it lowers from even when that DP is the cut. Acceptance: c2.f builds and prints 1; a caught text that allotted still refuses. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rw.
