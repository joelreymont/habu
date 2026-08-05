---
title: Model compiler ID allocator
status: active
priority: 1
issue-type: task
created-at: "2026-07-27T14:39:02.868347+02:00"
---

Claim: agent=proof_allocator workspace=.jj-ws/proof-allocator

Scope: define formal/Common/IdAllocator.v for the IR-0.1 process-wide module-serial allocator. Model the aligned atomic state, successful and failed CAS transitions, interleavings, and the 1..0x7fffffff serial domain. Declare the host atomic-CAS operation and its linearizability law as the only explicit external boundary unless an existing primitive proof is bound; this leaf does not own a committed assumptions report. Acceptance: Rocq 9.2 builds; transitions are executable; zero, out-of-domain, stale-CAS, and exhausted states decide correctly; no allocator laws beyond definitional examples; no Admitted. Ownership: formal/Common/IdAllocator.v only. Excludes checked implementation, ID packing laws, allocator theorems and their local assumption evidence, committed assumptions reports, parity, replay, arenas, dialects, native/GPU, and maki. Depends on habu-add-compiler-ir-21e976fc.

Checkpoint:

1. Owner: Rocq module `Habu.Common.IdAllocator`; corresponding `IR-ID` serial cell.
2. Entry: `formal/Common/IdAllocator.v` after `Habu.Common.Ids`.
3. Green: accepted IR-0.1 allocator/concurrency suite plus compiled `Ids.v`.
4. Red: run `rocq compile -Q formal Habu formal/Common/Ids.v`, then `rocq compile -Q formal Habu formal/Common/IdAllocator.v`; the second command fails because the allocator model does not exist.
5. Interface: aligned cell, CAS result and pure transition, guarded next step, attempt result, interleaved runner, and named host-CAS/linearizability boundary.
6. Forbidden: resettable or per-context serials, post-add overflow checks, wrapping, non-atomic transitions, hidden axioms, allocator theorems, Habu edits, or a committed assumptions report.
7. Focused: run `rocq compile -Q formal Habu formal/Common/Ids.v`, then `rocq compile -Q formal Habu formal/Common/IdAllocator.v`.
8. Broader: every current `formal/Common/*.v`, no-Admitted and assumption scans, host, stale-status, and dot dependency gates.

BLOCKER RESOLVED 2026-08-04 (dot-purge): `habu-add-compiler-ir-21e976fc` is closed and its edge removed here. The IR-ID module landed - `src/compiler/ir/id.f`, `test/compiler/ir-id.f` and `test/compiler/ir-id-concurrency.f` are present on the `proofs` branch and `proofs@origin` is at the same revision (1bb76eab), which is exactly the close condition that dot recorded for itself. The dependency is satisfied, not dissolved.
