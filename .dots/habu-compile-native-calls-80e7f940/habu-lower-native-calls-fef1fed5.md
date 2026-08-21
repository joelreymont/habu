---
title: Lower native calls
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:59:20.053554+02:00\""
closed-at: "2026-08-14T11:06:27.411491+02:00"
close-reason: "Closed as SATISFIED (acceptance audit 2026-08-14, clause-by-clause with registered dual-listed proof): calling convention = NABI contracts + A64RAV:ACCEPT production gate; clobbers = narrow-only rows with four E-NCLOB-WIDEN fixtures; targets = E-NMIGRATE-CALLEE with four negatives; signatures = E-NELAB-CALL/INLINE/QUOT + rc-70 fixtures; homes = instruction-count pins under pressure + SPILLS/REMATS pins + E-A64RAV-DKEEP structural guard; symbolic targets = RESOLVE-SCAN with the staleness case; typed execute = DO-EXEC certified arity, decoded to the engine's own execute. The blocks edge to 92993f27 was stale (closed). The one residual - the legacy staging road accepting a net-neutral effect lie (re-probed live today, no wrong answer producible) - is owned by habu-delete-the-callee-de637624."
---

Full context: design Wave 4 adds direct calls and typed indirect execute under explicit calling convention, clobber sets, stack homes, and symbolic targets. Acceptance: live-across-call values obey allocation validation; mismatched signatures/targets/clobbers reject; only required homes materialize.
