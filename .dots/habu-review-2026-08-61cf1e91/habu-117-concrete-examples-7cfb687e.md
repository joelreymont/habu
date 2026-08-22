---
title: 117 concrete Examples published as theorem rows
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.057389+02:00"
---

Problem: test/compiler/checker-model-axioms.txt lists 125 theorem rows = 8 universal Theorems (Effects.v:2453-2527, Control.v:2605-2668) + 117 vm_compute Examples; docs/proofs.md:8-12 forbids publishing example families in a manifest; the other six manifests publish Theorem/Corollary only (Interning 18 Examples, Storage 18, Structure 7, IdLaws 9, Ids 10, IdAllocatorLaws 4 - none published); of the 117 only the 31 vectors are cross-checked against the real checker. Acceptance: the manifest publishes the 8 theorems plus the gate-generated vector obligations; the rest demoted; docs/compiler-id-assumptions.md-style counts regenerated. Files: test/compiler/checker-model-axioms.txt, checker-model-manifest.f. Verify: proof slice. Depends: prover. Ownership: proofs. Claim: unassigned.
