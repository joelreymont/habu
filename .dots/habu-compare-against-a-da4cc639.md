---
title: Compare against a small literal without a register
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-02T11:27:29.112195+02:00\""
---

src/compiler/native/select.f EMIT-FLAG and EMIT-CMPBR always compare two registers, so a comparison against a literal - which is what 0= is (a hir.const 0 then hir.eq) and what every 'n =' against a small number is - spends one move-wide materialising that literal and one virtual register holding it. The machine has Cmp-immediate (src/arch/arm64/asm.f ENC-CMPI, a 12-bit unsigned field) and the engine's own 0= uses it. Add a64.flagi and a64.cmpbri forms carrying the literal as an attribute held against that field, and select them when a comparison's operand is an hir.const whose value fits and whose only use is that comparison. Wins one instruction and one register on 0= (1047 uses in the survey) and on every small-literal comparison. Found while landing habu-complete-the-comparison-63760034; the vocabulary is correct without it, this is the cost.

Claim: agent=cmpimm workspace=.jj-ws/habu-cmp-imm

Progress (cmpimm, 2026-08-12). LANDED in this workspace: a64.flagi and
a64.cmpbri, minted in the dialect (version 0.9 -> 0.10) and selected by the
COMBINE pass, not by the selector - combine.f is where the tree's other three
constant folds live and its header states why a selector that emits fewer
operations than it walks has to keep its own instruction accounting.

The literal rides under the EXISTING a64.off key rather than a new one. The
key's own note says a key is per FIELD, and ENC-CMPI drops its number into the
same twelve-bit unsigned no-shift field as ENC-ADDI and ENC-SUBI (one ?IMM12
between them), so a second key would be a second copy of one bound, free to
drift. Nothing in the tree classifies an operation by carrying a64.off - the
opcode answers "addend or comparison operand" for every reader.

Measured, corpus rows only: COUNT-DOWN 36->32, WS? 88->72, LADDER 168->144.
-44 bytes over three rows, every answer bit-for-bit, no untouched row moved,
codegen-compare 0 findings.

RESIDUE, measured and NOT taken: the comparisons this fold leaves are all
a64.cmpsel - the if-conversion turns a small two-armed body into a conditional
select, which is a third machine form the leaf does not name. SYM-FOLD-C keeps
2 and LADDER keeps 2 on that path, worth about 16 more bytes on scored rows.
An a64.cmpseli is the same machinery again (fold operand 1, three operands
instead of four); it needs its own leaf and its own measurement.
