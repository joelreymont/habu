---
title: Compile memory-access corpus words
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T11:44:38.775127+02:00\""
---

Phase 1 of the compiler-improvement plan: the chain cannot express @ ! c@ c! so CELL-BUMP (and, with locals, BYTE-SUM/BYTE-FIND) has no new column. Wanted: general load and store forms in A64IR - address from an SSA value, cell and byte widths, threaded on the generic memory token the data-stack forms already use - selected from HIR memory words the corpus body actually uses (read tools/codegen-compare-corpus.f CELL-BUMP first and build exactly its needs), allocated and validated as ordinary values, emitted through asm.f's real LDR/STR/LDRB/STRB encoders with their own bounds. Acceptance: CELL-BUMP compiled by the chain, executed identically on the pinned inputs (it mutates a cell - the harness's own fixture provides the cell), row in the table with honest total-per-call numbers. Mutations: wrong width, swapped address/value operands, token chain broken - die through execution or named refusal.

Claim: agent=memlane workspace=.jj-ws/habu-compile-mem-access-64ae47d3
