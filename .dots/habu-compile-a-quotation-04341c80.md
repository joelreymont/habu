---
title: Compile a quotation as a body in its own emission
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:44:39.220921+02:00"
---

S1 of the ruled quotations design (quotd lane 2026-08-10). A [: ... ;] body becomes ONE MORE FUNCTION of the module (IR supports D-FUNS=64; select/combine already loop FUN-COUNT) emitted as bytes of the enclosing routine's emission - no dictionary record, publish.f's ':-is-the-sole-constructor' stance untouched; the value is hir.quot (no operands, one code-ref result - ir/type.f:753 kind exists - attribute naming the callee function) lowered to a64.codeaddr = Adr rd, entry: PC-RELATIVE, so NO ADDRMAP bit, NO AOT capture site, no bb9b6d70 interaction (the engine's C-ADR precedent, P_pc_relative_adr in the frozen reloc schema; ENC-ADR and F-ADR rows already exist, Insn.v done). Work: lift the four FUN-COUNT!=1 refusals (regalloc.f:2113, regalloc-verify.f:296, spill.f:1022, emit.f:2136); hir.quot rows; a64.codeaddr rows; elaborator opens/closes a nested function on the tape; refuse inlining a body containing a quotation until measured. MEASURE adr's +-1MB reach against the largest realistic emission before landing; if thin, adrp+add (still position-independent). Acceptance: QP-ACT migrates and the returned xt EXECUTES; emission DECODED - exactly one adr targeting the body entry (text search is not proof); the three-quotation shape (sumtype.f:1445) migrates; past-reach fixture refused by name; [: in comment/string does not change function count; census [: bucket falls from 32. Files: src/compiler/native/{hir,hir-word,elaborate,a64ir,select,emit,combine,spill,regalloc,regalloc-verify}.f. Depends: none.
