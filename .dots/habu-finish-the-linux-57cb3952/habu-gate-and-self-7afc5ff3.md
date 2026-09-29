---
title: Gate and self-host the x86-64 engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.397922+03:00"
blocks:
  - habu-inventory-the-gate-9d02ff35
  - habu-pass-the-full-07316f5a
  - habu-self-host-the-ccc31e78
  - habu-measure-tier-1-0faadb01
---

Lane G: inventory the gate for a tier-1-only engine, pass the full gate on the cross-built engine on the ThinkPad, self-host to a byte fixpoint there (the release artefact), and measure tier-1 latency on spark (G4a) and on the ThinkPad (G4b, which opens the B2 follow-on).
Leaves: habu-inventory-the-gate-9d02ff35 (G1), habu-pass-the-full-07316f5a (G2), habu-self-host-the-ccc31e78 (G3), habu-measure-tier-1-f7425ab1 (G4a), habu-measure-tier-1-0faadb01 (G4b).
Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close.
Ownership: krait (Intel lane).
Claim: unassigned.
