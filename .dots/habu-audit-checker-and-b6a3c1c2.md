---
title: Audit checker and identity proofs for padding
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:55:03.554307+02:00"
---

Full context: apply AGENTS.md Proof Integrity (BLOCKING) to formal/Common/{Effects,Control}.v (44 and 68 published Examples, bound to src/core/checker.f) and the identity models Ids/IdAllocator/IdLaws/IdAllocatorLaws.v (49 results, bound to src/compiler/ir/id.f). Structural difference to handle: Effects.v and Control.v publish concrete Examples rather than universal theorems. A concrete Example CAN be load-bearing — it pins a specific decision the checker makes — but a family of Examples that all fail under one code mutation is ONE result wearing many names. Find those families and either collapse them into a universally quantified statement (much stronger) or keep a representative few. Keep the two documented identity axioms (host_atomic_cas, atomic_cas_linearizable) but check the results resting on them still constrain the code. WARNING from a sibling lane: bin/hb --load re-derives the checker from source, so many checker mutations break the engine's own load before the gate runs, giving exit 70 rather than a gate failure — that is a stronger fail-closed property, not a gap, but it means clean falsification needs semantically-inert mutations (reorder, rename a local, change a constant). Acceptance: every surviving result has a recorded falsifying code mutation; the checker-model and identity gates green.
