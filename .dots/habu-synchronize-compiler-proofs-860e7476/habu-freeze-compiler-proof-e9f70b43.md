---
title: Freeze compiler proof schemas
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.712252+02:00"
blocks:
  - habu-seal-compiler-ir-3c1e313d
---

Full context: design sections 10.1 and 10.6 require implementation/proof synchronization for every stable IR and witness schema. Define machine-readable manifests, canonical digests, Rocq record/inductive counterparts using only NEWTYPE/ENUM/STRUCTURE semantics, and parity checks. Acceptance: any field/opcode/version/order drift fails before proof or implementation publication; Rocq 9.2 builds with no Admitted.
