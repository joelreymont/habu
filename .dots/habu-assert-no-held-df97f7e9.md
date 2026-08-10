---
title: Assert no held migration crosses a capture edge
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:56:46.267597+02:00"
---

CAPTURE-REPL brackets ndict@ before/after its window; a migration left held-pending across an edge (or retracted mid-window) makes [R0,R1) describe records that no longer exist. Assert M-HELD-PENDING is zero at BOTH window edges in stdin.f's CAPTURE-REPL, refusing the capture otherwise by name. Also re-measure the four capture caps (BLOB 64KB, REC 256, SITE 2048, DSITE 512 - each overflows with a die) against chain-emitted window code when the cut nears. Files: src/habu/stdin.f, src/compiler/native/migrate.f (a reader for the flag). Depends: none.
