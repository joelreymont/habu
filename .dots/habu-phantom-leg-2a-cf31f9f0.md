---
title: "Phantom leg 2a: sink + ternary rep combinators"
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T13:04:15.297122+02:00"
---

Continuation of habu-ptx-phantom-preserving-3df9db92 leg 1 (commit 0360af4a, see its LEG 1 LANDED note for the design): extend lib/ptx/rep.f (package PTXREP) with the two cheap forge-proof combinator shapes that need NO new checker capability: (1) a SINK combinator family for phantom-consuming stores - effect shape ( .. [ n .. -- ] -- ) with the operand phantoms unified like REP2/REPMIX2 - converting ~13 TRUSTED sink wrappers to CHECKED callers: tile.f STORE STORE-ONCE SCATTER-ADD FANIN-SCATTER-ADD INDEX-DENSE-STORE INDEX-SCATTER-ADD INDEX-STORE, collective.f ROW-STORE ROW-STORE-ONCE ROW-SCATTER-ADD, tile-v4.f:21 STORE-V4, tile-v4a.f:58 STORE.V4, tile-smem.f:31 SSTORE; (2) REPMIX3 ( a b c [ n n n -- n ] -- a ) for the 3 ternary sites: tile.f:104 FMA., collective.f:98 BLOCK-MAX-SELECT, tile-acc.f:30 ACC-FMA. Same discipline as leg 1: bodies bare execute, unifier enforces forge/kind/arity, a negative regression per new combinator shape in rep-neg-test.f + positives in rep-test.f (wired into the same gate slices), byte-identity golden capture pre/post (the combinators emit nothing), TRUSTED rows for converted wrappers REMOVED (+N combinator rows), trusted-inventory ratchet down, trust-lint green, full battery incl. maki + gate-stdlib; run.f only if any engine prefix is touched (leg 1 needed none). Expected net roughly -12 to -14 trust sites. Files: lib/ptx/rep.f, rep-test.f, rep-neg-test.f, tile.f, tile-v4.f, tile-v4a.f, tile-smem.f, tile-acc.f, collective.f, TRUSTED.md, gate wiring. DISJOINT from the staging-decomposition lane (cg-matmul-emit/cg-matmul/cg-mma are fenced to it). Ownership: ptx trust retirement.
