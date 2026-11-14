---
title: Retire unsupported ?dup
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:04:08.142352+02:00"
---

Goal: retire the unsupported runtime word ?dup rather than add value-sensitive stack checking. ?dup duplicates a nonzero value and leaves zero alone, so its stack depth depends on the value. Checked definitions refuse it; Habu libraries and tools have no use beyond a rejection fixture.

Remove its runtime, compiler and primitive registrations (src/habu/prims.f:943, src/habu/habu1.f:4074, src/habu/kernel-x64.f:5464, src/core/checker.f:5531, :6873 QDUPBAD, :15546) and its obsolete special refusals, inspecting bootstrap and target implementations before edits. Keep public primitive IDs or metadata coherent at their existing owner; leave no dead implementation or compatibility alias. Application migrations belong to package owners if the consumer inventory finds any. Document the explicit checked conditional idiom: a word that prints only a nonzero number can use `dup 0<> if . else drop then`.

Acceptance: an executable zero/nonzero case and an undefined-word refusal remain in the real-load primitive and refusal tests; `: q ( option<n> -- ) ?dup drop drop ;` (`?dup` on a layout value; under `hb --load` today the bare `habu: in q: at '?dup'` with no reason, exit 70) reports E-UNDEFINED.
