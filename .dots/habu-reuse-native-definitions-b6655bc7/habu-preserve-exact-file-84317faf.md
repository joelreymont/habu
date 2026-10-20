---
title: Preserve exact FILE family identities
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T15:06:43.860955+03:00"
closed-at: "2026-10-20T18:19:57.784086+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Unchanged TASK callback-in-FILE E2E exports/runs but explicit import throws76. Independent Astra traced checker formatv10 (exact-prefix), empty family graph payload and callback EN-PARAM family171. CHECKER-UNIT-PREPARE unconditionally enables portable graph remapping although UNIT-NODE? collects referenced families only in portable mode; UNIT-FAM-MAP-ID then has no active map. Fix serialization/import mode consistency at checker boundary: established exact-prefix identities remain valid; portable family mapping and malformed-input refusal remain strict. Inspect exact-format new-family deltas before selecting conditional mapping. Keep original CALLBACK declaration inside FILE; chain and exit-hook images, portable checker-unit/family E2Es, automatic initializer and USER-slot cases must pass. No missing-map fallback or fixture weakening. Baked fix requires matching native/fullsuite/convergence and independent review.
