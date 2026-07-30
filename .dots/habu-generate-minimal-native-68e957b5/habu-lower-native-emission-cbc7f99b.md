---
title: Lower native emission through typed IR
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-18T11:52:19.524294+02:00\""
blocks:
  - habu-type-native-protection-c26d8323
  - habu-emit-proof-carrying-058f43b6
---

Compiler-IR reconciliation: this dot owns only the validated A64IR-to-A64ENC adapter for the Wave 2 subset and the first migrated compiler entry. Consume allocated A64IR plus its accepted witness, lay out symbolic labels/fixups, call the typed encoder, and construct deterministic object bytes and source maps. Do not define a parallel machine IR around the old emitter and do not require universal byte identity with the old compiler. Acceptance: one real checked SQUARE-shaped definition reaches valid bytes; malformed labels, fixups, effects, frames, transitions, witness bindings, and encoder outputs reject before executable publication; the migrated entry cannot bypass validated A64IR.

Claim released 2026-07-30: eleventh leaf of an eleven-leaf chain with all ten upstream leaves open (checkpoint report in this dot family); re-dispatch only after the allocation-witness leaf lands.
