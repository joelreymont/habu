---
title: Compile locals and division corpus words
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T12:49:32.660587+02:00\""
---

Phase 1 continues: LERP waits on locals and division; BYTE-SUM/BYTE-FIND wait on locals plus byte-width memory (dotted) plus their control flow (landed). Wanted: (1) Forth typed locals in the elaborator - read the exact LERP body in tools/codegen-compare-corpus.f first; a {: :} local in structured code is a named SSA value bound at declaration and read by name, needing NO memory unless something takes its address (refuse address-of by name if the corpus does not need it); locals crossing a control join travel as block arguments exactly like stack values, so the elaborator binds names to the value vector machinery it already has. (2) Signed division: hir.div selected to a64.sdiv, emitted through asm.f's real encoder (add ENC-SDIV beside the others if absent, pinned in insn-cases; check division-by-zero semantics - ARM64 sdiv yields zero, Forth / on the engine does what the old emitter does: compare both columns' behavior on the pinned inputs and if they diverge on any pinned input, report rather than paper). Acceptance: LERP compiled by the chain, executed identically on the pinned inputs, row in the table; the gap list for BYTE-* loses the locals entry. Mutations: local bound to the wrong stack slot, local read after rebinding, division operand order - die through execution or named refusal.

Claim: agent=locallane workspace=.jj-ws/habu-compile-locals-and-6aff8605
