---
title: Model JIT instruction encodings in Rocq
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-30T09:22:08.306131+02:00\""
---

Full context: PRIORITY 1 proof upgrade, directed 2026-07-30. The engine's assembler words in src/habu/habu2.f (MOVZ, MOVK, LDR, STR, ADD/SUBI, LSLI/LSRI/ASRI, AND/ORR, CMP, BCOND, B, BL, CBZ/CBNZ, RET, and the rest of the emitted vocabulary - enumerate from the source, do not guess) have no correctness statement: a field-packing mistake ships silently until a crash. Build formal/Common/Insn.v: an inductive of the emitted instruction forms with their operand fields, an encode function to 32-bit words matching the ARM64 encodings, a decode for the same subset, and theorems: decode after encode is identity on the vocabulary; field ranges refuse out-of-range operands the same way the shipped words do (compare against their guards); distinct forms encode distinctly. Bind with a parity gate (test/compiler/insn-proof.f following the checker-model-proof.f conventions): ONE shared vector table of (form, operands, expected 32-bit word) drives BOTH the Rocq obligations AND a Habu test that calls the REAL assembler words emitting into a scratch buffer and compares the emitted word - measure first how the emitter words write (CP-relative? buffer cell?) and use the real emission path, not a reimplementation (Test Integrity). Falsify by mutation: flip one field shift in one shipped assembler word, rebuild, the gate must red on exactly that form's rows; restore. Start with the forms the snapshot relocation pass depends on (BL, LDRB/STRB, LSLV, ORR) so this composes with the relocation round-trip proof (sibling dot), then grow to the full emitted vocabulary; record uncovered forms as MODEL GAPS rather than stopping the leaf.

Claim: agent=insnproof workspace=.jj-ws/habu-model-jit-instruction-7216ea39 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED, 2026-07-30. `bin/hb --load test/compiler/insn-proof.f` runs 330
assertions in 22 seconds and exits 0; `bin/hb --load test/compiler/insn-manifest.f`
runs the 262 of them that need no toolchain. `make -C formal` compiles
`formal/Common/Insn.v` in 5 seconds; all ten published results report "Closed
under the global context", and there is no `Admitted` in the file.

Where the assembler lives, measured before designing: `src/arch/arm64/asm.f`,
`icode.f` and `mnem.f` are LIBRARY Forth. The engine build appends them into the
stage source, but a test loads them from disk, so a mutation needs no fixpoint
rebuild - every falsification below was an edit plus a 22-second run.

Vector rows, 98 in all, one line per form (form: rows):
Movz 5, Movn 3, Movk 3, Add 2, Sub 2, And 1, Orr 2, Eor 1, Mul 1, Sdiv 1,
Udiv 1, Lslv 2, Lsrv 1, Addi 3, Subi 2, Andi 2, Orri 1, Eori 1, Lsli 3, Lsri 3,
Asri 2, Ldr 3, Str 2, Ldrb 3, Strb 2, Ldrw 2, Strw 1, Ldar 1, Stlr 1, Cmp 3,
Cmpi 3, Cset 2, B 4, Bl 5, Bcond 7, Cbz 2, Cbnz 2, Adr 2, Svc 2, Ret 1, Brk 1,
Nop 1, DsbIsh 1, Isb 1, Blr 1, Br 1, IcIvau 1, DcCvau 1. Plus 6 overflow rows,
18 reserved-register rows (12 refused, 6 not), and 4 logical-immediate packings.
Every expected word was taken from the ARM64 encoding and cross-checked against
clang -c -arch arm64 read back with objdump, so no row can agree with a bug in
the Habu encoders by construction.

Falsification matrix. Each mutation was applied to the shipped source, the gate
was run, and the source was restored byte-identically (`jj diff` empty after
every restore).

  M1  asm.f ENC-LSRI, shift amount field 16 -> 17    3 red, all Lsri rows
  M2  asm.f MOVZHW, hw field 21 -> 20                2 red, the two Movz rows
                                                     with a non-zero hw
  M3  asm.f ENC-CSET, condition field 12 -> 13       3 red, both Cset rows and
                                                     the Cset overflow row
  M4  asm.f ENC-LDAR, base register field 5 -> 6     3 red, the Ldar row and
                                                     the two Ldar x18 rows
  M5  icode.f D19, delta field 5 -> 6                8 red, every Bcond, Cbz
                                                     and Cbnz row with a
                                                     non-zero delta
  M6  asm.f XREG?, guard body deleted                12 red, exactly the twelve
                                                     refusal rows
  S1  one expected word in insn-schema.f changed     BOTH sides red: the Habu
                                                     row and the generated Rocq
                                                     obligation
  S2  a manifest statement weakened to `True`        Rocq refuses the pinned
                                                     definition
  S3  branch filler count off by one                 9 red on the row-length
                                                     check

A first attempt at M6 moved the guard from x18 to x19 instead of deleting it.
That reported zero failures, because three vectors legitimately use x19 and the
moved guard ended the gate process before its first assertion. Recorded in
LESSONS.md.

FINDINGS, all pinned by committed rows and by theorems in Insn.v, each with a
dot:

  1. No encoder bounds any operand. MOVZ, with 65536 emits `movz x0,#0,lsl #16`;
     CSET, with condition 16 emits `cset x1,eq`; ADDI, with 4096 and ADD, with
     register 32 emit words that are not any instruction the model names. Dot
     habu-refuse-out-of-3536f1ed.
  2. The scaled mnemonics round down silently. MOVK, divides its shift by 16 and
     LDR, divides its byte offset by 8 with a plain Forth `/`, so a shift of 8
     becomes 0 and an offset of 12 becomes 8. Same dot.
  3. Four forms take an X register the reserved-register check never reaches.
     ENC-LDAR and ENC-STLR do not call XREG? at all, and CBZ,/CBNZ, in icode.f
     build their word without going through the ENC-CBZ/ENC-CBNZ encoders that
     would have. ENC-B, ENC-BL, ENC-BCOND, ENC-CBZ and ENC-CBNZ are dead in the
     native path; they exist for parity with the Gforth seed generator. Dot
     habu-guard-x18-in-7cc74c05.
  4. `LSLI, rd rn 0` and `LSRI, rd rn 0` are the same word, so a left shift of
     zero is outside `wf`. Published as a counterexample rather than a defect.

MODEL GAPS: the fourteen floating-point encoders (dot
habu-model-the-arm64-2906d23f), the `>LIMM` mask synthesis whose packed result
the logical-immediate forms take as an operand (dot
habu-model-the-logical-5f2be671), and the unchecked TRUST boundary the gate
calls the standalone assembler through (dot habu-give-the-standalone-8cc02435).
The B/BL out-of-reach refusal (?REL26) cannot be reached at all through the 2 MB
code window, so only the model covers it.
