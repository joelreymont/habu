---
title: "Checker reentrancy: certify defs compiled during word execution"
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T15:14:54.739645+02:00"
---

BLOCKER for habu-compiler-pkg-scoped-1a4d29bd full static composition. When an executing word triggers compilation of a new colon definition WHILE the check hook (set-check HOOK / CHECK!) is active, the native engine CRASHES (SIGBUS / EXC_BAD_ACCESS, jump to PC=0x1 via blr x9 from clobbered interp state [x20,#0x1b0]). Reproduced with BOTH evaluate and included. Minimal reproducer (bin/hb --load): "0 set-check : W ( -- ) s\" : ZZ ( -- n ) 5 ;\" evaluate ; 1 set-check  W" -> EXC_BAD_ACCESS. Also crashes with included of a file defining a colon word. Top-level evaluate of the same colon def (not nested inside a word execution) is fine, so the gap is checker/compiler NON-REENTRANCY across a word-execution boundary: the check hook and interpreter/compile state are not saved/restored for a nested definition. Capability needed: make definition compilation + certification reentrant (save/restore checker arenas + interp compile pointers), OR provide a supported native primitive to compile-and-certify a definition programmatically from within an executing word. Until this lands, MODEL: (an executing word) cannot compile its body as a checker-verified colon definition. Owner: src/core/checker.f + native colon compiler + src/core/check-hook.f. Evidence gathered on fable host 2026-07-06 (lldb).
