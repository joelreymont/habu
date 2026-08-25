---
title: Retire the derived terminator field
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:48:27.772660+02:00"
---

Full context: found by the Rocq structure proof. src/compiler/ir/fun.f BROW-ADD writes the terminator ordinal as the operation window's last index, and TERMINATOR@ recomputes exactly that and rejects anything else. The stored field is therefore a function of the operation window, so on any constructed row the comparison can never fail — it is a second authority that is definitionally in agreement, which is precisely the shape the same file's header (lines 73-76) rejects for the parent field. Theorem stored_terminator_is_derived in formal/Common/Structure.v. Either drop the field and compute the terminator, or document why a redundant second authority is wanted here when the file argues against exactly that elsewhere. Acceptance: the decision is implemented and the header no longer contradicts itself.
