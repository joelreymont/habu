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

DESIGN VERIFIED AGAINST THE TREE AT d3a9c848 (2026-08-05, remat2), and one
question the design does not answer found. Every site the design names is still
where it says, so the plan above is buildable as written: MB-COPY? :1210 with
SAME-SYM?, MB-SPILLABLE? :1364/:1366, MB-FRAMED? :1508/:1511, MB-DUE?
:1590/:1594, MB-EVICT :1763 with NEW-SLOT at :1766, MB-FINISH :1786/:1792,
MB-PLAN-STORES :1814, MB-PLAN-LOADS :1822 with P-RELOAD planned at :1828,
BIND-DIALECT :2003. A64IR-OPCODE:MOVZ/MOVK exist (a64ir.f DEF-MOVZ :1082,
DEF-MOVK :1098) and the movk tie is stated at :251-259. spill.f already copies
K-IMM and K-SHIFT (COPY-ATTRS), and EMIT-LOAD's shape - build a fresh value,
RBIND it at the position - is exactly the shape EMIT-REMAT needs, with no slot
and no memory token. Plan rows carry blk/kind/pos/val only (PLAN+ :402), so a
P-REMAT row needs the immediate carried per class or re-read from the module.

THE UNANSWERED QUESTION IS THE VALIDATOR'S LINK. The design says
regalloc-verify.f "re-derives per site that the re-emitted chain computes the
class's constant". A64RAV:VERIFY (:1982) is handed ONE function - the module as
it stands - and FLOW-CK (:719) ties a reload to its store through the shared
SLOT attribute. A remat site has no slot, so that link does not exist, and a
movz carrying the WRONG immediate is still a well-formed module: standalone,
the validator cannot tell it from the right one. Before building, decide which
of these the link is - a class identity carried on the remat op (which risks
the validator trusting the plan it is supposed to re-derive), the pre-spill
module handed to VERIFY alongside the lowered one, or a rule that all movz ops
reaching one reader must agree - because the mutation test the acceptance
demands ("mutate the emission, the validator must refuse") is only meaningful
once it exists.

AND A FACT THAT CHANGES WHAT PIECE 1 BUYS: A64SPILL:REWRITE IS NOT ON THE
PRODUCTION PATH. migrate.f EMITTED (:567-572) runs SELECTED, A64RA:ALLOCATE,
A64RAV:ACCEPT, A64EMIT:EMIT and never rewrites; A64RA:SPILLS is read only by
spill.f itself, test/compiler/native-regalloc.f and
test/compiler/native-chain-fixture.f, and A64SPILL:REWRITE is called only from
those fixtures. So on the corpus the allocator either fits or refuses, and no
corpus row's bytes can move because of a spill or a remat. Remat's corpus
effect is confined to turning refusals into compilations; the four-row CSE win
does not depend on it, and RSPILL-CASE - which is what blocks the CSE - reaches
remat through the fixture path. Confirm this before pricing piece 1 against the
corpus, because the consolidation block prices it as if the two were coupled.

Claim: agent=remat2 workspace=.jj-ws/habu-fold-constants-and-cbe4e25e

DEFERRED (2026-08-05, generation 3): remat is premature until the spill rewrite loop is on the production path (see the cut dot — production currently fits-or-refuses; A64SPILL:REWRITE is fixture-only, so corpus remat effects are confined to refusal-flips). Also unresolved and BLOCKING the acceptance's mutation test: the validator has no link from a remat site to the class's constant (FLOW-CK ties reload to store through the slot attribute; a remat site has no slot — a wrong-immediate movz is a well-formed module). Three candidate links recorded by the lane: class identity on the remat op (validator would trust the plan it should re-derive — weakest), handing VERIFY the pre-spill module too (honest independent re-derivation — orchestrator's leaning), or an all-movz-to-one-reader-must-agree rule. Decide when the rewrite loop lands; do not build remat before both.
