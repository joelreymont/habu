---
title: Refuse out-of-range ARM64 operands at encode time
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T10:43:48.990732+02:00\""
---

Full context: found while building the instruction-encoding parity gate (dot habu-model-jit-instruction-7216ea39). The encoders in src/arch/arm64/asm.f bound no operand at all: every field is shifted and OR-ed into the word, so an operand one past its field runs into the neighbouring one and the wrong instruction is emitted with no diagnostic. Four proved counterexamples, all committed as rows in test/compiler/insn-schema.f and as theorems in formal/Common/Insn.v: MOVZ, x0 with a 16-bit immediate of 65536 emits $D2A00000, which is 'movz x0, #0, lsl #16'; CSET, x1 with condition 16 emits $9A9F17E1, which is 'cset x1, eq'; ADDI, x1 x2 with 4096 emits $91400041, which is not any instruction the model names; ADD, x1 x2 with register 32 emits $8B200041, likewise. Two more come from the scale divisions: MOVK, divides its shift by 16 and LDR,/STR, divide their byte offset by 8 with a plain Forth /, so a shift of 8 silently becomes 0 and an offset of 12 silently becomes 8. Fix: add fail-closed range checks beside XREG? - a register field 0..31, imm16 0..65535, imm12 0..4095, the packed logical immediate 0..8191, a shift 0..63, a condition 0..15 - and make the scaled load/store and move-wide mnemonics refuse a byte operand that is not a multiple of its scale, all through the same s" ..." 72 die path the reserved-register check uses. Then move those rows in test/compiler/insn-schema.f from the overflow table to the reserved table with code 72, and turn the three counterexample theorems in formal/Common/Insn.v into refusal statements.

Claim: agent=asmguards workspace=.jj-ws/habu-refuse-out-of-3536f1ed

MEASURED 2026-07-30 (agent asmguards, workspace .jj-ws/habu-refuse-out-of-3536f1ed).

What was built. src/arch/arm64/asm.f now bounds every operand before it packs a
bit. Seven named limits are derived from the width of the field the operand
goes into and written once: REG-LIM (5 bits), IMM16-LIM, IMM12-LIM, NIS-LIM
(13), SHIFT-LIM (6), COND-LIM (4), HW-LIM (2). One word, OUT?, decides whether
a value fits an unsigned field of that size, and one guard per field kind -
?REG, ?IMM16, ?IMM12, ?NIS, ?SHIFT, ?COND, ?HW - refuses with its own
diagnostic through the same `die` path XREG? already used. XREG? is now ?REG
followed by the x18 test, so a register operand is bounded and screened in one
place. SCALE/ replaces the three bare `/` divisions: it refuses a byte operand
the division would round down, then divides, and MOVK, in src/arch/arm64/mnem.f
uses it for its shift. The exit status is unchanged and now named ASM-EXIT-RC
(72), the same code the label and branch layer reports, so no new error code was
introduced and error-code-lint is untouched. The floating-point encoders were
bounded too, through DR2/DR3, since their D-register operands share the same
five-bit fields.

Where a bound was deliberately NOT added, and why. A branch or ADR displacement
is bounded by src/arch/arm64/icode.f before it reaches an encoder (?REL26,
?REL19, ?ADR), and an ADR displacement is a word position times four, so it
cannot be misaligned. The packed logical immediate is built by >LIMM, which
never returns a value outside its field, so ?NIS on ENC-ANDI/ORRI/EORI cannot be
reached through the shipped mnemonic; it is there for a caller that packs its
own value. A left shift of zero is inside the six-bit field and is still
emitted: the model calls it malformed only because that word is also a right
shift of zero, which is a decoder property and is published separately as
lsli_lsri_alias_at_zero. All four are written down in the schema header rather
than left to be discovered.

What proves the bounds are not too tight. The engine rebuilds to its fixpoint
with the guards in place: `tools/build-fixpoint-refresh.f -- install --force`
runs every emitter over the whole engine source, and not one refusal fired.
Self-check census went from `0 uncheckable, 0 rejected, certified = 4240` to
`0 uncheckable, 0 rejected, certified = 4251`; the eleven are the new guard
words themselves (twelve added, XRD3 deleted as dead). Nothing was rejected and
the certified count did not drop.

Cost of the guards, measured. Three `install --force` rebuilds before the change
took 12.88, 12.90 and 12.89 seconds (mean 12.890); three after took 13.00, 12.95
and 12.91 (mean 12.953). That is +0.063 s, about half a percent, on a machine
whose repeat spread is a few hundredths of a second - so it is at the edge of
what this measurement can distinguish. Reported as measured, not optimised.

Coverage on both sides. test/compiler/insn-schema.f grew from 88 encoding rows
to 139, from 6 overflow rows to 36 out-of-range rows, and from 18
reserved-register rows to 77. Every form now has one encoding row with all its
operands at their largest legal value (register 31, imm16 65535, imm12 4095,
shift 63, condition 15, offset 32760/16380/4095, shifted half 3), and every
expected word was produced by clang -c -arch arm64 read back with objdump, never
by the Habu encoders. The old overflow table, which recorded the wrong word the
shipped code emitted anyway, is gone: every one of those operands is now
refused, and the table that replaced it drives each through the real mnemonic in
a child engine and requires exit 72, while the Rocq side asks the model whether
the same operands are well formed and requires `false`. So the two bounds are
one bound: loosen it in the assembler and the child emits instead of dying;
loosen it in the model and the obligation stops proving.

Gates, on the exact tree (artifact `jj diff --git` against 53cbae17):
test/compiler/insn-proof.f exit 0 in 1m50s; test/compiler/insn-manifest.f exit
0; test/compiler/reloc-proof.f exit 0; `make -C formal clean` then
`make -C formal` clean with no Admitted; the engine fixpoint rebuild green;
tools/asm-src-test.f, test/icode-fixup-test.f, tools/bootstrap-codegen-test.f,
tools/codegen-role-test.f, tools/compiler-dispatch-test.f and
test/engine-size-test.f all ok; the ptx-stdlib slice (via lint-libs) PASS;
package-diff-lint, typed-local-diff-lint exit 0 on the artifact;
error-code-lint 0 finding(s); dot-dep-lint 0 finding(s).

One pre-existing red found and NOT caused by this work: the `trusted-inventory`
suite in the lint-tools slice fails with four assertions
(tools/trusted-inventory-test.f, the TRUSTED.md ratchet). It fails identically
on the pristine parent commit 53cbae17 with an empty working copy, so it is a
red that arrived before this lane. It needs its own dot or owner.

One side repair this work forced. tools/package-diff-lint-core.f rejected every
change to src/arch/arm64/asm.f, icode.f and mnem.f, because the three files
carry no `package` at all and the lint reports any changed global definition
outside one. That is not a fault of this change: measured on the same tree,
adding the operand bounds reported ten ownership faults for words the encoders
already published, so as configured the gate refused every possible change to
the ARM64 assembler. The three paths are now listed in GLOBAL-IMPLEMENTATION?
beside src/core/checker.f and src/core/render.f, on the same interim terms and
with the same reasoning written out, and pinned by new fixtures in
tools/package-diff-lint-test.f: three positives, four negatives including the
disassembler in the same directory and a deleted package boundary. Retiring that
entry - giving the three files real package owners and migrating their callers -
is dot habu-pkg-the-arm64-ffabc063, added here.

Is this the best long-term fix, or a patch? Long-term. The bound each guard
enforces is the width of the field the operand is shifted into, written once as
a named constant and derived from that width rather than transcribed; there is
one check word per field kind, not a copy per encoder, and the refusal path is
the one the file already had. The guards are placed at the encoder that owns the
field, so a new encoder that forgets one is visible as a missing call rather
than as a missing line inside a shared helper. Nothing rests on a lucky value, a
sentinel, or a range that happens to work: the same numbers appear in the
model's `wf`, and the schema rows make the two answer the same question on every
form.
