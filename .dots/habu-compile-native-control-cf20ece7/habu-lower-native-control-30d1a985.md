---
title: Lower native control flow
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:59:20.000633+02:00\""
closed-at: "2026-08-14T11:51:17.260773+02:00"
close-reason: "Closed SATISFIED with the mechanism drift recorded honestly (Wave-3 audit 2026-08-14): the leaf prescribed validated fixups and layout-independent labels; the shipped design DELETED fixups (one-pass layout+encode, nothing to patch) and made layout single-owner cursor-validated (E-A64EMIT-LAYOUT before any caller reads a byte) - measured stronger, per Simplify Relentlessly. Branch ranges validate before encoding (E-A64EMIT-REACH, predicates pinned at exact edges, Adr impossibility derived and asserted). Branch-collapse landings are this machinery maturing. Residue owned: e23caccb (emitter reach end-to-end)."
blocks:
  - habu-build-native-loop-71d4a638
---

Full context: lower verified control-flow SIR through LIR/A64IR using symbolic branches, layout-independent labels, and validated fixups. Acceptance: branch ranges/layout/fallthrough/terminator/one-exit invariants validate before encoding; no semantic state is recovered from bytes.
