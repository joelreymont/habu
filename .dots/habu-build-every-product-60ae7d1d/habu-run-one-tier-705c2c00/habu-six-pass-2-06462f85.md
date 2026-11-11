---
title: Six pass-2 checker words and their primitive rows have no caller
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.235418+02:00"
---

Problem: found by the JIT list census (.jj-ws.noindex/jerry-wasm/.out/jit-list-report.md, findings outside the brief): src/core/checker.f :20895-20922 P2LW, P2SEQ, P2-LOCSEQ-RESET, P2-CARVE-W, P2-LIVE-W@, P2-LIVE-CUM@ and their PRIM rows :13138-13141 have no caller in any case; the comments at :20471-20472 and :20905 say the engine calls them through the dictionary, but habu2.f EM-P2-CARVE-W reads the LOWER-CERT certificate instead. Acceptance: the six words, their rows and the two comments are deleted. Files: src/core/checker.f. Verify: rg -i --no-ignore for each name finds nothing; the engine builds to a fixpoint. Depends: none (the JIT lanes may take it). Ownership: unassigned. Claim: unassigned.
