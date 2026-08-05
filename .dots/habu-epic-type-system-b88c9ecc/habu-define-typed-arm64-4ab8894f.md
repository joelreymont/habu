---
title: Define typed ARM64 routine effect schema
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T11:44:21.963217+02:00\""
blocks:
  - habu-link-arm64-contracts-8cca6cc1
---

Context: tools/lint/clobber-lint.f RETURNS-MASK, PRESERVE-MASK, EFFECTS, and PSEUDO-EFFECTS hardcode partial name and opcode tables. Forth stack effects describe the host emitter stack, not generated register state. Cause: callable emitted labels have no first-class machine-state contract. Fix: add a package-scoped checked ARM64 effect schema for GPR and SIMD live-ins, reads, writes, returns, preserves, NZCV, LR, SP delta and alignment, typed frame slots, no-return behavior, syscalls, BL, and BLR. Infer contracts where possible and require declarations at irreducible boundaries; missing, duplicate, or contradictory contracts reject. Replace handwritten routine-name switches with schema queries and link each primitive row to the audited primitive-effect axiom table. Acceptance: every callable native and recovery emitter label has one validated contract; mutation fixtures for each effect field reject; schema and bootstrap parity gates are green; no new unchecked boundary is introduced.

Claim: agent=a64effects workspace=.jj-ws/habu-define-typed-arm64-4ab8894f

Progress 2026-07-30 (landed as commit 50557aac): the schema record itself has
landed - package A64EFF in src/compiler/a64-effect.f with suite
test/compiler/a64-effect.f, 36-mutation matrix all caught. Still owed before
this dot closes: a registry so every callable emitter label has exactly one
contract and a missing or duplicate declaration is refused; the census giving
each existing native and recovery emitter label its contract; and replacing
the hand-written routine-name switches in tools/lint/clobber-lint.f with
registry queries. The registry needs a decision about who owns a routine's
name identity - this schema or the A64IR dialect's symbol table - so it was
left to the leaf that can answer it. Binding a contract to a target contract
waits on the target-policy owner.
