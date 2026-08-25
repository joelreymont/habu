---
title: Model Ldp and Stp so pairs can be emitted
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T18:08:53.977107+02:00"
---

Measured, not speculative: tools/codegen-combine-inventory.f counts 25 adjacent same-base load pairs one slot apart across the 54 migrated corpus rows (723 emitted instructions), and 0 store pairs. The load pairs concentrate in the rows that read many locals - MANY-LOCALS 7, WIDE-ARITY 4, CALL-LOOP-3 3, T-SET 2, VEC-COPY-CELLS 2 - which is the data-stack and frame traffic a call site and a prologue emit, exactly as the combining leaf predicted. Folding them would take about 25 instructions off the corpus, on top of the 22 the multiply-add combine already took.

BLOCKED ON MODEL ROWS, WHICH IS THE POINT OF THIS DOT. formal/Common/Insn.v models 48 forms and Ldp/Stp are not among them, so no emitter may write one: the CG-02 discipline is that the row lands before the pass that uses it, the way Smulh/Madd/Msub did in 44bdfe6b. Work: add Ldp and Stp constructors with enc, wf, decoder rows and roundtrip lemmas; note that their offset field is a SEVEN-bit SIGNED immediate scaled by the access width, which is a different shape from every unsigned-offset form already modelled, so the existing scaling helpers do not carry over. Then the shipped encoders in src/arch/arm64/asm.f, their rows in test/compiler/insn-schema.f, an a64.ldp/a64.stp pair of opcodes and schemas in src/compiler/native/a64ir.f, and an arm in src/compiler/native/combine.f beside the multiply-add one.

The pairing predicates are already written and tested: NCOMBINV:LDP-PAIR? and STP-PAIR? in tools/codegen-combine-inventory.f, with fixtures for the cases that must NOT pair (two slots apart, different base, one register written twice, and a load that overwrites the base the next load reads). A pass reusing them starts from a measured count rather than a guess.

Acceptance: the standing optimisation-lane terms - name the rows before implementing, show the emitted instruction delta, every answer bit-for-bit, both gaps reported, chain baseline re-pinned deliberately after the report is read.
