---
title: Prove compiler ID schema
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T13:22:24.314793+02:00"
---

Full context: coordinate the stage-local proof of IR-0.1 only: freeze its checked canonical ID manifest and reachable numeric vectors, model exact 64-bit identities and predicates in Rocq, prove bounded packing laws, separately model and prove the process-wide CAS allocator, then bind digest, numeric parity, static wrong-family rejection, require replay, and the explicit atomic-CAS assumption. Children own all implementation. Dependency: habu-add-compiler-ir-21e976fc. Acceptance: all six bounded leaves land; Rocq 9.2 builds with no Admitted; the canonical digest, reachable valid/hostile decisions, static schema distinction, allocator laws, and replay fixture agree; the assumptions report exposes atomic-CAS linearizability. Excludes shared records, tables, opcodes, general witnesses, dialects, native/GPU theorems, and maki/infer.

BLOCKER RESOLVED 2026-08-04 (dot-purge): `habu-add-compiler-ir-21e976fc` is closed and its edge removed here. The IR-ID module landed - `src/compiler/ir/id.f`, `test/compiler/ir-id.f` and `test/compiler/ir-id-concurrency.f` are present on the `proofs` branch and `proofs@origin` is at the same revision (1bb76eab), which is exactly the close condition that dot recorded for itself. The dependency is satisfied, not dissolved.
