---
title: Write the checked ownership model
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-16T13:54:54.703368+03:00\""
closed-at: "2026-09-29T10:19:18.160793+02:00"
close-reason: Define the C2 scope, view, storage, unwind and consumer contract in docs/ownership-model.md; reconcile language references and roadmap. Oracle findings verified against checker/runtime and pinned Tender source, corrected, and followed up with no remaining design blockers. Design only; implementation and runtime acceptance remain open on c2.
---

Problem: the old borrowing proposals disagree about lifetime-bearing raw pointers,
scope escape and exceptional owner restoration. Existing spans and rigid identities
do not establish borrowing safety. Tender requires independently mutable readers
over shared immutable package bytes and trees that retain source slices after a
reader closes.

Acceptance: docs/ownership-model.md specifies lexical owner/loan authority,
generative effect binders, typed borrowed storage and projections, raw-boundary
admission, stale-state unwind, task cleanup and image capture. It includes the
OPC/XML/tree/owned-document flow and positive/negative implementation criteria.
Oracle review runs before production C2 coding; confirmed findings are resolved
and independently checked. docs/forth.md, docs/type-system.md and the C2 roadmap
link the same contract and distinguish planned support from shipped behavior.
The campaign uses that delivery order, without universal region-pointer, linear
locals or downstream stable-release prerequisites. Linear phases wait for a
concrete consumer; this design task does not implement or rename production APIs.

Files: docs/ownership-model.md, docs/type-system.md, docs/forth.md,
docs/roadmap.md and this campaign's design records.
Verify: focused design review against the current checker/runtime and immutable
Tender consumer source; Oracle findings verified against those paths.
Depends: none. Ownership: checker lane. Claim: alder design review.
