---
title: Extend the immediate fold to logic and compare
status: active
priority: 2
issue-type: task
created-at: "2026-08-07T13:36:13.421214+02:00"
---

Audit: ~96 gap bytes across 10 rows are constants built in registers where the instruction takes an immediate — and/orr/eor bitmask immediates (TAG: mov #7;and vs and #7), cmp/cmn immediates (SYM-FOLD-C, WS?, LADDER guards), movn for -1 (BYTE-FIND wastes 12 bytes building it with movz+3 movk), lsl for *2 (CALL-FAN). The add/sub immediate fold landed in the combine pass (2605426e) — extend it to the logical/compare families the encoders already ship (verify each ENC-* exists; bitmask-immediate encoding is its own validity check — refuse unencodable masks, never approximate). Also LERP's latent selection waste: a runtime zero-divisor check (mov #100;cbnz;brk = 12 bytes) on a CONSTANT nonzero divisor — fold the check when the divisor is a known nonzero constant. Measure-first per row, answers bit-for-bit, boundary fixtures straddle, deliberate re-pin.

Claim: agent=immext workspace=.jj-ws/habu-extend-the-immediate-e3e8378f

Progress (immext, 2026-08-07). LANDED on bookmark immext: the movn chain
(BYTE-FIND 96->84) and the bitmask fold for and/orr/eor (TAG 16->12,
SYM-FOLD-C 40->36, CALL-FAN 44->40). Four rows, -24 bytes, every answer
bit-for-bit, no untouched row moved, chain baselines re-pinned deliberately.

STILL OPEN, with what the tree says about each:

- lsl for a power-of-two multiply (CALL-FAN). ENC-LSLI ships (asm.f:247, LSL
  via UBFM) and Insn.v carries Lsli, so this is IR-side only: an `lsli` opcode
  plus a combine rule folding a movz whose value is a power of two into a
  reading O-MUL. WARNING: test/compiler/native-inline.f:1012 derives its L1..L4
  inline-size boundary from "a multiply by a constant is a move-wide and a
  multiply, two instructions". That sentence stops being true and every link
  length must be re-derived - the fixture survives today only because its two
  multiplies are by different PRIMES, so check that before assuming green.

- cmp/cmn immediates (WS?, SYM-FOLD-C, LADDER guards). Bigger than it reads.
  This dialect has no standalone cmp: comparison reaches the machine only as
  the fused forms flag (a64ir.f:373), selz, cmpsel and cmpbr, so the work is
  immediate variants of THOSE (a64.flagi, a64.cmpbri), not one `cmpi` opcode.
  ENC-CMPI ships (asm.f:263) and Insn.v carries Cmpi. ENC-CMNI does NOT exist
  and Cmni is not in Insn.v either, so cmn needs the rows-first treatment first
  (the SMULH commit 6e47c8e0 is the shape) - or drop cmn, since cmn #k is
  cmp #-k and the corpus rows measured are all cmp.
  OVERLAP: .dots/habu-compare-against-a-da4cc639.md is open, unclaimed, and
  specifies exactly this (a64.flagi / a64.cmpbri in select.f EMIT-FLAG /
  EMIT-CMPBR). One of the two leaves should be retired rather than both worked.

- LERP's constant-divisor guard. Not a fold of a constant into a field at all.
  a64.sdiv is ONE operation that emits THREE instructions - cbnz, brk, sdiv
  (emit.f PUT-SDIV, a64ir.f DEF-SDIV declares SET-TRAP true). So the shape is a
  second opcode, a divide whose divisor is proven nonzero, emitting the sdiv
  alone and declaring no trap; the combine pass rewrites sdiv to it when
  operand 1 is defined by a movz with a nonzero value. The movz STAYS - sdiv
  has no immediate form - so the saving is the 8 bytes of cbnz and brk, not 12.
  NOTE: test/compiler/native-select.f keeps a branch alive precisely BECAUSE
  "one arm divides and a division may trap"; clearing the trap flag on the new
  opcode can eat that fixture, so re-derive it in the same change.

Also filed: habu-pkg-the-src-5c3e9049, for the LIMM? unit rows that could not
land because tools/asm-src-test.f is unpackaged.

PARTIAL LANDED 2026-08-07 (merged from immext): movn materialisation (cost comparison in select.f MATERIALISE, tie to movz) and the and/orr/eor bitmask fold (LIMM-TRY/LIMM? packer split — the mask bound IS the packer, unencodable masks unrepresentable in the IR; the 5-vs-7 fixture kills range-check impostors). -24 bytes over 4 rows, predictions exact on TAG and BYTE-FIND. REMAINING, designs in the lane's report: lsl (IR-side; re-derive native-inline L1-L4 boundaries), LERP's proven-nonzero divide (8 bytes, trap-flag fixture care). The cmp-immediate clause of this leaf is CEDED to habu-compare-against-a-da4cc639, which specifies it exactly (a64.flagi/a64.cmpbri variants of the fused forms; ENC-CMNI/Cmni need rows-first or dropping) — one owner, not two.
