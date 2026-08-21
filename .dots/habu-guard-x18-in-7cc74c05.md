---
title: Guard x18 in LDAR, STLR, CBZ and CBNZ
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-30T10:43:29.739027+02:00\""
---

Full context: found while building the instruction-encoding parity gate (dot habu-model-jit-instruction-7216ea39). src/arch/arm64/asm.f opens by promising that the Darwin-reserved register x18 is refused at encode time 'for every X-register operand field', and XREG? does that for the shifted-register, immediate, load/store, move-wide, compare and indirect-branch encoders. Four emitted forms escape it. ENC-LDAR and ENC-STLR (src/arch/arm64/asm.f, the two lines that read '5 lshift or $C8DFFC00 or MSK') never call XREG? at all. CBZ, and CBNZ, in src/arch/arm64/icode.f build their word directly from $B4000000/$B5000000 rather than going through ENC-CBZ/ENC-CBNZ, so the XR2ND check those encoders do carry never runs; ENC-B, ENC-BL, ENC-BCOND, ENC-CBZ and ENC-CBNZ are in fact dead in the native path and exist only for parity with the Gforth seed generator in bootstrap/cg/asm.fs. Evidence: test/compiler/insn-schema.f rows F-LDAR 18 15, F-LDAR 14 18, F-STLR 18 5, F-CBZ 18, F-CBNZ 18 all emit a word and exit 0, while every guarded slot exits 72; formal/Common/Insn.v proves unguarded_x_register_forms, which says these four are exactly the modelled forms whose checked operand list differs from their X-register list. Fix: route the acquire/release encoders through XR2 and give the icode branch emitters the same check (either by calling the asm.f encoders or by adding XREG? at the icode site), then flip those five schema rows from an emitted word to exit code 72 and update checked_regs in the model so unguarded_x_register_forms becomes 'no form'.

Claim: agent=asmguards workspace=.jj-ws/habu-refuse-out-of-3536f1ed (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED 2026-07-30 (agent asmguards, workspace .jj-ws/habu-refuse-out-of-3536f1ed).

The choice the dot left open - route the direct builders through the encoders,
or delete the dead encoders and guard the builders - was settled by routing, and
here is why. src/arch/arm64/icode.f says in its own first line that it emits
"ARM64 words (asm.fs encoders)", and ADR, in that same file already builds its
word by calling the encoder with a zero displacement and letting the fixup layer
fold the real one in. B,, BL,, BCOND,, CBZ, and CBNZ, were the only emitters
that did not. They now do the same thing ADR, does: the base word is
`0 ENC-B`, `0 ENC-BL`, `0 I-COND @ ENC-BCOND`, `I-RD @ 0 ENC-CBZ` and
`I-RD @ 0 ENC-CBNZ`. Deleting the encoders instead would have left icode.f
holding five ARM64 opcode constants that asm.f also knows about; routing removed
$14000000, $94000000, $54000000, $B4000000 and $B5000000 from icode.f entirely,
so each opcode's bit layout is written in exactly one place, the five encoders
stopped being dead, and the register and condition-code refusals for those forms
now happen where every other form's happen.

ENC-LDAR and ENC-STLR now call XR2, so both the transfer register and the base
register are screened. ENC-BCOND gained ?COND, and every register operand is
also field-bounded now, because XREG? is ?REG followed by the x18 test (see dot
habu-refuse-out-of-3536f1ed).

The model moved with the code. formal/Common/Insn.v `checked_regs` now lists
[rt; rn] for Ldar and Stlr and [rt] for Cbz and Cbnz, which makes it identical
to `xregs`, and the published result changed shape accordingly: the theorem that
used to be `unguarded_x_register_forms`, whose statement was the LIST OF
EXCEPTIONS, is now

    every_x_register_is_checked : forall i, checked_regs i = xregs i

- one universal statement with no exceptions, proved by `destruct i;
reflexivity`. test/compiler/insn-axioms.txt carries the new name and the new
type row, so Rocq's own type checker has to accept the proved statement as the
one the manifest wrote down.

Coverage. The five schema rows the dot named (F-LDAR 18 15, F-LDAR 14 18,
F-STLR 18 5, F-CBZ 18, F-CBNZ 18) flipped from an emitted word to exit code 72,
and F-STLR 14 18 was added because the old table did not have it. Beyond those,
the reserved-register table now has one row for EVERY X-register operand slot of
every modelled form - 76 of them, up from 17 - because `checked_regs` is a claim
about each slot on its own and a row is what makes that claim answerable.
Deleting XR3 from ENC-SUB, for instance, now reds three named rows instead of
none. The one remaining row that is not refused is the control: an immediate
that happens to be the number 18, which still encodes, so a check that refused
the number rather than the register would not look correct.

Falsification, run on this tree. (1) Removing XR2 from ENC-LDAR reds exactly
four rows - the two Ldar x18 rows and the two Ldar out-of-range rows - and
nothing else. (2) Reverting CBZ, to build `$B4000000 I-RD @ or` directly reds
exactly one row, the Cbz x18 row, and nothing else. (3) Setting
`checked_regs (Ldar rt rn)` back to `[]` makes formal/Common/Insn.v stop
compiling at every_x_register_is_checked. All three were restored and the tree
re-verified green afterwards.

Gates: the same set recorded on dot habu-refuse-out-of-3536f1ed, all green on
this tree.

Is this the best long-term fix, or a patch? Long-term. The invariant is
structural, not a value test: for a form to be guarded, its word must be built
by the encoder that owns that opcode, and after this change there is no other
way to build one - the opcode constants no longer exist outside asm.f. That is
what makes `checked_regs = xregs` maintainable rather than a snapshot, and it is
why the fix is a routing change rather than a second copy of XREG? added at the
icode call sites. The alternative (guarding the builders in place) would have
left two files that both know what a CBZ word looks like and two places a guard
has to be remembered.
