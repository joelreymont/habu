---
title: Rematerialize constants instead of spilling them
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T17:41:33.933692+02:00"
---

Problem: src/compiler/native/regalloc.f treats a value defined by hir.const like any other spill candidate, so a constant that loses its register costs a store, a load and a frame slot. On AArch64 a small integer literal is one movz with no operands, so re-emitting it in front of the reader is strictly cheaper than the stack round trip, and hir.const is the canonical rematerializable value: src/compiler/native/hir.f DEF-CONST declares it with no operands, one result of the schema's fixed cell type, PURE-VALUE, and SET-TRAP false, so re-emitting it anywhere in the routine computes the same thing. Evidence, measured 2026-08-05 on the literal-CSE lane: with block-local constant CSE enabled, tools/codegen-compare.f improves four of 41 rows and regresses none (CALL-FAN-BIG 88 to 56 bytes, TINY-CALLEE 96 to 80, CALL-FAN 48 to 44, CALL-LOOP-3 112 to 108), but test/compiler/native-chain.f RSPILL-CASE throws -8329 E-A64RA-PRESSURE: NCH-RSPILL compiled into its deliberately starved frame (4 registers, 1 slot) needs two spilled values instead of one because collapsing the two literal 1s stretches that constant's live range across the add chain. Minus one movz, plus one spill. Fix: mark values defined by an operand-free pure non-trapping opcode as rematerializable; when the allocator would spill one, record a remat decision instead of a slot decision, and have src/compiler/native/spill.f insert a fresh defining operation in front of each reader rather than a store and a load. src/compiler/native/regalloc-verify.f re-derives spills from operations and must learn the remat shape in the same change, and src/compiler/native/frame.f stops counting a slot for it. Acceptance: RSPILL-CASE passes unchanged at RSPILL-SLOTS 1 with constant CSE on, still spilling above the link slot and still answering 54/54/14; no corpus row regresses; regalloc-verify accepts the rewritten module and a mutation that drops the remat re-emission reds a focused fixture. Files: src/compiler/native/regalloc.f, src/compiler/native/spill.f, src/compiler/native/regalloc-verify.f, src/compiler/native/frame.f and their focused suites. Depends: none. Blocks: habu-literal-cse-trips-7e6d67bb and through it habu-fold-constants-and-cbe4e25e. Ownership: register allocation spill and rematerialization decisions. Claim: unassigned.

Consolidation (2026-08-05): one remat design serves two consumers — this dot (hir.const: operand-free, pure, non-trapping, so re-emitting the movz always beats a stack round trip; unlocks the literal-CSE leaf habu-fold-constants-and-cbe4e25e whose measured win is 4 rows improved 0 regressed, gated only by the RSPILL pressure case) and habu-rematerialize-the-loop-1faad3e1 (pure loop-invariant loads, the PRESSURE-LOOP road). The remat machinery in regalloc.f/spill.f/regalloc-verify.f/frame.f is shared; build it once with constants first (simpler purity argument), loads second, one lane. The validator re-derives remat correctness independently: a remat'd value equals what the original op computes, checked per site.

DESIGN CORRECTED BEFORE BUILD (2026-08-05, remat). Two facts read off the tree
falsify the premise this dot and its consolidation block are written on. A
worker starting from the text above would build the wrong thing, so the
correction is here rather than in a report.

FIRST: BY THE TIME THE ALLOCATOR RUNS, A CONSTANT IS NOT AN OPERAND-FREE
OPERATION. src/compiler/native/regalloc.f runs after selection, and
src/compiler/native/select.f MATERIALISE (:1254) lowers one hir.const to a chain
- a64.movz for the lowest half, then one a64.movk per further non-zero half, up
to four instructions. a64.movk READS the value the previous half left and
defines the merged value (src/compiler/native/a64ir.f:251-259), so the machine
module has no operand-free constant at all. The dot's phrase "operand-free, pure,
non-trapping" is true of hir.const in the SOURCE dialect and false of what the
allocator sees. What survives is better: a64ir.f states that the two SSA values
are one register field and movk's schema says so with a TIE, so the whole chain
is ONE register class. One class is one materialised constant, and
rematerialising a class means re-emitting its chain.

SECOND, AND IT CHANGES THE ACCEPTANCE: RE-EMISSION IS NOT ALWAYS CHEAPER THAN A
FRAME ROUND TRIP. A spill costs one store plus one load per use site; remat
costs the chain length N per use site, N in 1..4. For N=1 remat strictly wins -
it replaces the load, drops the store, and takes no slot, which is the RSPILL
case (literal 1) and the CALL-FAN-BIG case (literals 3 and 5). For N=4 it
replaces one load with four instructions at every site, and
tools/codegen-compare-corpus4.f BIG-CONSTS (:337) is exactly that shape: four
distinct sixty-four-bit literals per turn, each already documented in that file
as a chain of four move-wide instructions. Blanket remat would regress that row,
and "no corpus row regresses" is this dot's own acceptance.

So the rule is not "constants are rematerializable". It is: REMATERIALISE A
CLASS WHEN RE-EMITTING IT COSTS NO MORE THAN THE RELOAD IT REPLACES - chain
length one - and spill the rest. That is a derived criterion, not a threshold
somebody picked: it compares the two costs the allocator is choosing between.
Whether to widen it to length two, which still drops the store and the slot,
is a measurement on the corpus and not a judgement to make in advance.

THE DESIGN THAT FOLLOWS, IN THE FILE'S OWN IDIOM. regalloc.f already learns
opcode identities from the dialect rather than spelling them: BIND-DIALECT
(:2003) binds A64IR-OPCODE:MOV into BND-MOV and MB-COPY? (:1210) compares with
SAME-SYM?. Bind MOVZ and MOVK the same way and the candidacy test is structural:
a class is a remat candidate when every value in it is defined by movz or movk
and exactly one of them is the movz that roots it.

The blocking structural detail is that CL-SLOT currently carries two meanings at
once. `CL-SLOT <> NOSLOT` is read as "this class is in the frame" at five sites
- MB-SPILLABLE? (:1366), MB-FRAMED? (:1511), MB-DUE? (:1594), MB-FINISH (:1792)
and MB-PLAN-STORES/LOADS (:1819, :1828) - and remat needs a class that is
EVICTED but holds no slot. So the change is to split the meaning: keep CL-SLOT
for real frame slots only, add a per-class remat mark, and give the five sites
one shared CL-EVICTED? predicate. Then MB-EVICT (:1764) branches - a candidate
sets the mark and never calls NEW-SLOT, so frame.f counts no slot without being
touched - MB-PLAN-STORES skips remat classes, and MB-PLAN-LOADS plans a new
P-REMAT row where it would have planned P-RELOAD. spill.f re-emits the chain at
each remat site instead of inserting a load, and regalloc-verify.f re-derives
per site that the re-emitted chain computes the class's constant, never trusting
the plan.

NOT BUILT. This lane recorded the corrected design and stopped there; nothing in
src/ or test/ changed. The next attempt starts from this text, not from the
paragraph above it.

Claim: agent=remat workspace=.jj-ws/habu-fold-constants-and-cbe4e25e
