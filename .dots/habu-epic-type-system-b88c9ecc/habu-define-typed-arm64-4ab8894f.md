---
title: Define typed ARM64 routine effect schema
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T11:44:21.963217+02:00"
blocks:
  - habu-primitive-effect-axiom-1119f176
---

Context: tools/lint/clobber-lint.f RETURNS-MASK, PRESERVE-MASK, EFFECTS, and PSEUDO-EFFECTS hardcode partial name and opcode tables. Forth stack effects describe the host emitter stack, not generated register state. Cause: callable emitted labels have no first-class machine-state contract. Fix: add a package-scoped checked ARM64 effect schema for GPR and SIMD live-ins, reads, writes, returns, preserves, NZCV, LR, SP delta and alignment, typed frame slots, no-return behavior, syscalls, BL, and BLR. Infer contracts where possible and require declarations at irreducible boundaries; missing, duplicate, or contradictory contracts reject. Replace handwritten routine-name switches with schema queries and link each primitive row to the audited primitive-effect axiom table. Acceptance: every callable native and recovery emitter label has one validated contract; mutation fixtures for each effect field reject; schema and bootstrap parity gates are green; no new unchecked boundary is introduced.
