---
title: Prove A64 object semantics
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.781657+02:00"
blocks:
  - habu-retire-native-byte-f44351f7
  - habu-prove-lir-and-ef2a64ff
---

Full context: connect validated A64IR, layout/fixups, typed encoding, HBOBJ, relocations, executable loading, and loaded-image semantics for covered instructions. Acceptance: golden/corrupt vectors and composed theorem reach encoded bytes and loaded image; external loader/OS axioms are named exactly.
