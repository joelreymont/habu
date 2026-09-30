---
title: Declare and select x86 scalar float forms
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.484363+03:00"
---

Problem: `src/compiler/native/x64ir.f` declares no float forms; selection refuses with `E-X64SEL-FLOAT`. First half of the float work (dialect and selection); C7b emits and executes.
Acceptance: `x64ir.f` gains the SSE2 forms (dialect MINOR bump) for HIR `fconst fadd fsub fmul fdiv fneg fabs fsqrt flt fgt feq fltz feqz intreal realint bitsreal realbits` (`src/compiler/native/hir.f:81-97`); `select-x64.f` lowers each; XMM allocation through the fpr file; proven through allocation and validation.
Files: `src/compiler/native/x64ir.f`, `src/compiler/native/select-x64.f`, `src/arch/x86-64/machine.f`, `test/compiler/x64-select.f`, `test/compiler/x64-regalloc.f`.
Verify: spark `bin/hb --load test/compiler/x64-select.f`, `test/compiler/x64-regalloc.f`, `test/compiler/x64ir.f`.
Depends: habu-emit-x86-frame-d8d25223 (C1).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-declare-and-select-bfbb301b.

Preflight corrections (2026-09-30; override the lines above where they differ):
1. Forms. `x64ir.f` declares, on FPR-TYPE: `movsd` (copy), `addsd subsd mulsd divsd andpd xorpd` (tie 0 0, as DEF-BINARY `x64ir.f:966-980`), `sqrtsd`, `cvtsi2sd`, `cvttsd2si`, `movq-xr`/`movq-rx` (fconst, bitsreal, realbits; fneg/fabs = movi mask + movq + xorpd/andpd), `fstore`/`fload` (`asm.f:555-558`), and `fcmpset ( fpr fpr -- gpr )` with `x64.cond` in {gt, equal}: `flt` selects `gt` with operands swapped; `equal` carries a second unread GPR result (idiv precedent, `select-x64.f:60-61`) for `setnp`, since ucomisd sets ZF=PF=CF on unordered and HIR compares answer false on NaN (`hir.f:904-905`, `select.f:4-7`); `fltz`/`feqz` select a zero double first (no zero form, unlike `hir.f:919-921`). MINOR 2; OPCODES (`x64ir.f:420`) and the five MATCHes (`:366,:423,:472,:834,:885`); cond comment `:114-117`. `realint` is a bare `cvttsd2si` (ARM64 is bare fcvtzs, `habu1.f:3500`, `select.f:2773`; `asm.f:575-577` defers range/NaN to lowering and no doc defines it).
2. LOWERING names `fstore`/`fload` (`x64ir.f:823-824`, comment `:802-810`): every X64ABI contract clobbers the whole XMM file (`abi.f:63`), `MB-FORBID-CALLS` (`regalloc.f:1643-1660`) sends a double live across a call to the frame, and `spill.f:363-370` refuses without the pair. `test/compiler/x64ir.f:234-236` flips.
3. Files add `src/compiler/native/emit-x64.f` (`PUT-OP :707-758` is an exhaustive MATCH, `docs/forth.md:463`; each new form gets an `E-X64EMIT-FORM` arm until C7b), `docs/x86-64.md` (`:224-227,:313,:391-392`), `lib/errors.f` (`:1227` E-X64SEL-FLOAT becomes unthrown: remove it if nothing else throws it; `:1241` the `E-X64EMIT-FORM` comment, which also still lists "a trap, a code address", stale since C5). `lib/errors.f` is in the engine closure (`native-runtime.f:98`): Route becomes "lands on master after the Linux proof: rebuild with master's product, the five-generation chain, the gate on `hb-b5`; Alder pools the Mac gate".
4. The vocabulary has one `copy` (`dialect.f:121`, `x64ir.f:792`), so `MB-COPY?` (`regalloc.f:1277`) never coalesces the FPR tie copy; accepted here. No `movq` GPR<->XMM encoder exists in `asm.f`; C7b's Files add `asm.f` and `test/compiler/x86-64-asm.f`.
5. Verify: `x64-select.f` replaces FLOAT-REFUSE-CASES (`:1307-1309`; the `intreal realbits` fixture `:497-510` is the pre-change seam, green today) with one case per op, the tie copy, and the swap; `x64-regalloc.f` gains a mixed-file case and a double across a call accepted by A64RAV (shape `native-regalloc.f:3330-3470`). `test/compiler/x64-select.f`, `x64-regalloc.f`, `x64ir.f` and `x64-emit.f` run on the ThinkPad under qemu; then the spark rebuild, chain and gate for `lib/errors.f`. `machine.f` needs nothing (REGFILE already carries the file, `x64ir.f:349-357`).
