---
title: Reduce parametric payload width soundness question
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T14:43:29.624739+02:00"
---

Measured by the C12 lane, pre-existing: TX:idem-key, a FOUR-cell record family, is ACCEPTED as the type argument of tx-result (arity 1, recorded WIDTH 2 = tag + ONE payload cell), verdict -1 through the production checker; TX:txn (another record) is refused as a type argument. Either instantiation recomputes width per type argument (then the recorded WIDTH is a misleading registry fact and the accept is sound), or a four-cell payload is admitted into a one-cell slot (a layout soundness hole). Behavior: reduce to a minimal checked fixture - construct tx-result<idem-key> with a real idem-key, MATCH it back, and inspect the cells that come out; compare against a one-cell argument; read the width the registry reports for the instantiated type versus the family. Classify per the checker-miss taxonomy and either fix (with negative regression) or document the per-instantiation width rule in docs/type-families.md with a positive fixture. Owner: src/core/type-family.f width/layout layer. Dependencies: none. Priority 1 if the reduction shows cell truncation.
