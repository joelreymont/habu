---
title: Bind compiler ID parity
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-27T13:50:44.124249+02:00\""
blocks:
  - habu-freeze-compiler-id-2dc2c646
  - habu-prove-compiler-id-cfe76485
  - habu-prove-compiler-id-4fe3b4ea
---

Claim: agent=bind_parity workspace=merged

Implementation complete and merged onto the proofs bookmark; the parity gate
test/compiler/ir-id-proof.f passes and was independently verified non-vacuous by
mutating MODULE-MAX in src/compiler/ir/id.f (gate exit 1). Awaiting master
fast-forward before closure. Three follow-ups are dotted separately:
habu-pin-rocq-theorem-f03a68d5, habu-cover-all-identity-daa614eb,
habu-pin-parity-vector-5c762b01.

Scope: fan in the completed checked ID manifest/vector artifact, completed bounded packing laws, and completed allocator laws. Bind the Rocq schema digest to the exact checked Habu digest, run every shared runtime-valid/hostile numeric vector through both executable predicates, and prove that Habu `MODULE-MAX`, `LOCAL-MAX`, `LOCAL-BITS`, the aligned `NEXT-SERIAL` state, `SERIAL-NEXT`, and `TRY-SERIAL` correspond to the Rocq constants, guarded next step, CAS result, and attempt transition. Habu-side mutations of each constant, the exhaustion-before-add guard, CAS operands/success test, and retry result must fail this correspondence gate. Prove separately that Habu `COUNT`/`COUNT-N` and `POOL-OFF`/`POOL-OFF-N` agree with the Rocq scalar constructors/projectors over discriminating high-bit values and never acquire module bits. Bind the Habu checker wrong-family negative separately to Rocq type/schema distinction, and run require-replay as an executable child load-path fixture rather than a numeric predicate. Compare the complete Rocq assumption set against a committed expected-external-axiom manifest; the final committed report must expose the reviewed host-CAS operation and atomic-CAS linearizability law with no hidden replacement, unbound theorem, or Admitted. Acceptance: digest equality, numeric decision/diagnostic-class parity, Habu-source/Rocq allocator correspondence, scalar identity parity, static wrong-family parity, allocator-law binding, require-replay, and exact expected-assumption equality pass; a schema byte, vector decision, Habu constant/CAS transition, scalar packing, checker rejection, replay reset, unexpected or missing assumption, unbound theorem, or Admitted mutation fails. Full prerequisites: habu-freeze-compiler-id-2dc2c646, habu-prove-compiler-id-cfe76485, and habu-prove-compiler-id-4fe3b4ea. Ownership: parity runner/gate, replay fixture, committed expected-external-axiom manifest, and final committed compiler-ID assumptions report only. Excludes theorem-local assumption evidence, shared records, tables, opcodes, general witnesses, dialects, native/GPU, and maki.

Checkpoint:

1. Owner: proof-gate package `COMPILER-ID-PROOF`, binding `IR-ID` to `Habu.Common.Ids`, `IdLaws`, `IdAllocator`, and `IdAllocatorLaws`.
2. Entry: `require src/compiler/ir/id.f`, the compiler-ID child-load fixture, and all four formal files under `rocq compile -Q formal Habu`.
3. Green: all three prerequisite leaves pass their focused commands.
4. Red: no common digest/vector/source-correspondence gate, committed expected-external-axiom manifest, or final report exists, so an Habu-only CAS or scalar-packing mutation can pass Rocq.
5. Interface: one checked parity runner, one child replay fixture, one committed exact external-axiom manifest, and one deterministic final assumptions report; no runtime compiler API.
6. Forbidden: source-text-only equality without executable Habu mutations, copied vectors, packed scalar roles, numeric wrong-family/replay rows, assumption-subset checks, generated uncommitted allowlists, hidden trust, or Admitted.
7. Focused: compile all four Rocq files, then `bin/hb --load test/compiler/ir-id-proof.f`.
8. Broader: complete Rocq proof, compiler-ID, typed-local, package, refine, error, suite coverage, host, stale-status, dot dependency, Maki, PTX standard library, required fixpoint/bootstrap, and native publication gates.
