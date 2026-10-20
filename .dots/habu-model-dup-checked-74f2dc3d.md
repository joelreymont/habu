---
title: Retire unsupported ?dup
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:04:08.142352+02:00"
---

Decision: retire the unsupported runtime word after the current build work, rather than add value-sensitive stack checking. ?dup duplicates a nonzero value and leaves zero alone, so its stack depth depends on the value. Checked definitions currently refuse it; Habu libraries and tools have no use beyond a rejection fixture.

Remove its runtime/compiler/primitive registrations and obsolete special refusals, inspecting bootstrap and target implementations before edits. Keep public primitive IDs or metadata coherent at their existing owner; do not leave a dead implementation or compatibility alias. Application migrations belong to package owners if the consumer inventory finds any.

Document the explicit checked conditional idiom: a word that prints only a nonzero number can use `dup 0<> if . else drop then`. Inspect and revise existing real-load primitive/refusal coverage before implementation; retain an executable zero/nonzero artifact and undefined-word refusal. Run the affected target and primitive gates, full native suite and generation convergence, plus recovery if the seed/mirror changes; obtain independent Astra review. Keep this separate from the incremental-build correctness lane.
