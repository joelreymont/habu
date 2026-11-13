---
title: Store a cell family with arguments where an arity-0 one is stored
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T00:38:32.022080+02:00"
---

Problem: with NEWTYPE hnd<a>, [type] hnd<n>, [type] ptr hnd<n> and ARENA AH hnd<n> are refused, while hnd0 (arity 0) is admitted at width 8 and a STRUCTURE field of hnd<n> or option<hnd<n>> is admitted; hnd<n> is one cell either way. Cause: NOM-SCALAR? (src/core/checker.f) answers false for a family of arity above 0, so STORAGE-CELL-W (:8296-8304) and STORAGE-PTR-POINTEE-OK? (:8288-8291) refuse it as the stored type and as a pointee; the gate is at master a90eca37. Found by the review of the [type] fold (chain ooxylnop). Acceptance: a closed cell family with arguments is stored and pointed at wherever an arity-0 cell family is, and the MD-TYPE-FORM suggestion (src/core/render.f:1298) drops its 'no cell family with arguments' clause. Design first: read NOM-SCALAR?'s 7 callers (rg -i 'NOM-SCALAR\?' src) for what arity 0 protects; an open argument stays refused by the closed-type rule. Files: src/core/checker.f, src/core/render.f, test/compiler/width-type.f or the arena tests. Verify: [type] hnd<n> width>n gives 8 and ARENA over hnd<n> loads, in a test beside the arity-0 case. Depends: the stored-type chain (tlklkoxp) landed. Ownership: carl. Claim: unassigned. Consumers: no NEWTYPE with arguments in Maki, Zen or Tender trunk (rg, 2026-11-12); waits behind the consumer-met work (AGENTS.md).
