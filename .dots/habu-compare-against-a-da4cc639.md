---
title: Compare against a small literal without a register
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T11:27:29.112195+02:00"
---

src/compiler/native/select.f EMIT-FLAG and EMIT-CMPBR always compare two registers, so a comparison against a literal - which is what 0= is (a hir.const 0 then hir.eq) and what every 'n =' against a small number is - spends one move-wide materialising that literal and one virtual register holding it. The machine has Cmp-immediate (src/arch/arm64/asm.f ENC-CMPI, a 12-bit unsigned field) and the engine's own 0= uses it. Add a64.flagi and a64.cmpbri forms carrying the literal as an attribute held against that field, and select them when a comparison's operand is an hir.const whose value fits and whose only use is that comparison. Wins one instruction and one register on 0= (1047 uses in the survey) and on every small-literal comparison. Found while landing habu-complete-the-comparison-63760034; the vocabulary is correct without it, this is the cost.
