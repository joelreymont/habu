---
title: "Widen a routine's place list past ten arguments"
status: open
priority: 3
issue-type: task
created-at: "2026-08-03T12:24:18.907657+02:00"
---

Measured while building the fourth codegen corpus. A routine's calling convention is an ordered place list packed into ONE cell: src/compiler/a64-effect.f gives each position six bits (a five-bit payload and one bit saying register or data-stack slot) and keeps the length in the four bits above them, so SEQ-MAX-N = (64-4)/6 = 10 positions. A definition of eleven inputs is therefore refused with E-A64EFF-SEQ before any stage of the chain looks at its body, and ten is the ceiling for outputs too. The engine's emitter has no such limit. Checked, on the real migration entry, in tools/codegen-compare-test.f REFUSAL-CASES: an eleven-argument body throws -8209 and the same body with ten arguments compiles. It kept a wide-arity gap row out of tools/codegen-compare-corpus4.f, whose WIDE-ARITY row is six arguments for this reason. What to build: a place list that is not one cell - an arena row, or a cell plus an overflow span - so the ceiling comes off the packing. Owners: A64EFF, NABI.

POPULATION UPDATE (recorder landing 2f988d14): the measured 151
tranche adds 4 first-refusals E-A64EFF-SEQ (-8209) - long bodies
whose arity exceeds the ten-place packed list (src/compiler/
binding.f SAME? 528B leads). The class this leaf owns now has
real census rows beyond the corpus probe.
