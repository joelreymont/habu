---
title: Bind every clobber-lint call to an analysed routine
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T23:48:04.776501+01:00"
---

Problem: 20 of 123 'L... LABEL@ BL,' calls bind to label entries no analysed routine fills, so APPLY-CALL (tools/lint/clobber-lint-core.f ~1251) applies no clobbers there: four routines open through a local or a conditional (LFLUSHTO habu1.f:4449, LGENIOOUT habu2.f:369, LEXITHOOK habu2.f:1462, LPROFJSON/LPROFDUMP prof.f:634), and C-LABEL$ (~670) ignores using and never reads src/habu/prof-abi.f (LHEX, LPROTWIDQ, LHIDXADD, LBCS, LFLUSH, LPLACE, PROF-ABI:LPROFRECLAIM/LPROFTRIM). Acceptance: the lint fails when a call names no routine pass 1 opened; it resolves names as Habu does; every call binds to an opened routine and the lint passes, or each clobber it then reports is fixed in the engine. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-bind.
