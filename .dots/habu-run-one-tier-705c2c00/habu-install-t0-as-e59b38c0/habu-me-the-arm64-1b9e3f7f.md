---
title: "ME: the arm64 semicolon recovers refused definitions in a multi-error run"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T17:22:04.339088+02:00"
---

Workspace .jj-ws.noindex/jerry-me on ulzktzrp (R1). Brief /tmp/jerry-big/t0-me-recover-brief.md. habu2.f EM-COMPILE's ; sets NCOMP-DISPATCH:RECOVER-CELL around its compile under catch and abandons on E-NCOMP-RECOVERY, as definers.f DEF-SEMI?. Repro: /tmp/jerry-big/me/multi-err-t1.f on public hb dies -8572 (t1.out); JIT output jit.out. Fixes engine and type-decl rows (MEA1, TDSME1) once tier 0 is native.
