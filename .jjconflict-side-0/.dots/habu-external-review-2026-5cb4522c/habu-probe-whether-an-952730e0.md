---
title: Probe whether an unreserved arena append run can be caught
status: open
priority: 3
issue-type: task
created-at: "2026-08-20T21:09:51.052806+02:00"
---

Open QUESTION, not an approved build. Lane ir-1 made every multi-cell IR row reserve its whole storage before its first cell (habu-a-mid-row-e02603c6, src/compiler/ir/arena.f RESERVE). That invariant is now a discipline stated in arena.f and obeyed at every call site, but nothing refuses a NEW word that appends several cells to an IR-ARENA without reserving first - the next table added to src/compiler/ir or src/compiler/native can reintroduce the exact defect. PROBE FIRST, per the Simplify Relentlessly rule: (1) does the checker already have, or nearly have, a way to state 'this word calls PUSH more than once without a preceding RESERVE on the same arena'? (2) can the existing check-before-write reader in test/compiler/ir-storage-cases.f - which already classifies every definition in arena.f and context.f as writer/guard closed under calls, and already proves a guard precedes the first write - be pointed at the CALLERS instead, making this an extra row rather than a new tool? (3) is a lint even the right shape, or should PUSH itself require evidence of a reservation? Do not mint a new lint until one of these is answered with a failing probe through a real gate AND a named first consumer. Files: src/compiler/ir/arena.f, test/compiler/ir-storage-cases.f, test/compiler/ir-storage-schema.f. Depends: habu-a-mid-row-e02603c6.
