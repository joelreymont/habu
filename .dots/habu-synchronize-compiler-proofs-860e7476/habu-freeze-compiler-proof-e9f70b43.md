---
title: Freeze compiler proof schemas
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.712252+02:00"
blocks:
  - habu-encode-compiler-ir-545ee6d1
---

Full context: after the shared facade is sealed, freeze the remaining shared IR and witness schemas required by design sections 10.1 and 10.6. Define machine-readable manifests, canonical digests, Rocq record/inductive counterparts using only NEWTYPE/ENUM/STRUCTURE semantics, and parity checks. IR-ID and its ID manifest, digest, parity, and vectors belong only to habu-prove-compiler-id-399232c5. Acceptance: any remaining field/opcode/version/order drift fails before proof or implementation publication; Rocq 9.2 builds with no Admitted.
