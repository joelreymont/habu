---
title: Hoist and unroll the small loops
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.378317+02:00"
---

Loop-invariant code motion and bounded unrolling on the frozen IR: hoist pure computations and loads the memory order proves invariant out of loop bodies (the schema effect flags + the existing memory-order machinery decide legality); unroll counted loops whose trip count is a small literal or whose body is under a derived size bound (derive the bound from the I-cache line economics the placement doc measured, not a guess). The corpus's byte loops and the workload's scan shapes are the witnesses. Acceptance: measured against the clang column per row; answers identical; the register pressure interaction with unrolling is held by the existing pool floors (an unroll that would spill is declined, stated in the pass).

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: LICM only when typed memory/effect facts prove invariance and alias safety; unroll only when the measured size/runtime tradeoff is favorable and the allocator accepts the pressure, threshold derived from target cache/sequence costs.

Claim: agent=hoist workspace=.jj-ws/habu-hoist-and-unroll-7ef5b4c5 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED FIRST (2026-08-06, on master 36959656). The instrument is
tools/codegen-loop-inventory.f plus its runner and
test/compiler/codegen-loop-inventory.f, reading emitted code through the tail
probe's walk exactly as the combining lane's inventory does. Over the 54
migrated corpus rows: 701 instructions, 16 rows hold a loop, 16 loops, 162
instructions inside loop bodies.

  invariant LOADS: 0. Across the whole corpus. Not one loop carries a load
  whose address the body leaves alone in a body that writes no memory. The
  corpus loops are tight by construction - their invariant values (a, c, u,
  len, lr, base, cell) are locals already in registers, every address
  computation is a function of the induction variable, and every load is at a
  varying address. So the load half of LICM has NO consumer in this corpus and
  is not built. PRESSURE-LOOP, the one row whose body is entirely invariant, is
  also the one row the chain refuses to compile (E-A64RA-SPILL), and hoisting
  would not change that: its 14 loaded values are all live at the first add
  whether that add sits in the loop or in the preheader, so code motion moves
  the pressure without reducing it. That row stays the remat leaf's.

  invariant CONSTANTS: 42 move-wide instructions inside loop bodies, but they
  split two ways and only one way is this leaf's. 24 of them are a LONE movz of
  a small number feeding an arithmetic instruction - the chain selects the
  register-register form of an add or subtract where src/arch/arm64/asm.f
  already carries the immediate form (ENC-ADDI). Hoisting one saves an
  instruction a turn and costs a register held across the loop; selecting the
  immediate form saves the instruction and costs nothing, so a cheaper transform
  owns those 24 and the inventory counts them in their own column. That leaves
  18 genuine 64-bit literal chain members, and 16 of the 18 are in ONE row.

NAMED ROW AND PREDICTED DELTA, registered before any pass is written:

  CODEGEN-CORPUS4:BIG-CONSTS - four distinct 64-bit literals XORed with the loop
  index, each a movz plus three movk, all four rebuilt on every turn. Body 28
  instructions of which 17 are move-wide, 16 of them chain members. Hoisting the
  four chains into the loop's existing outside predecessor predicts body 28 ->
  12, a 57% cut in per-turn work, with chain BYTES UNCHANGED (the chains move,
  they do not duplicate) and 4 more values live across the loop against a pool
  of 18. Baseline to beat: chain 156 b / 7.979 ns, clang 392 b / 2.561 ns, gap
  5.418 ns. CODEGEN-CORPUS4:FLOAT-MIX is the only other row with a genuine
  chain member (2 instructions a turn) and is too small to name a delta for.

  No preheader has to be minted: the loop header's sole non-latch predecessor
  already exists and already dominates it, so the hoist lands in an existing
  block and the combine pass's one-for-one block copy carries over unchanged.
  A64SEL:SPECULABLE? (select.f:2147) is the ready-made legality predicate - it
  asks the schema for pure and non-trapping, which is the same question a hoist
  onto the zero-trip path asks - so no new legality machinery is needed either.

UNROLLING: not built, and the measurement says why. No corpus loop has a
literal trip count, so the leaf's "trip count is a small literal" arm matches
nothing and only factor-unrolling with a remainder is available. The bodies are
5 to 28 instructions, the loops are 16 in 54 rows, and the two rows where loop
overhead dominates most (SUM-TO body 5, COUNT-DOWN body 6) are the two where
clang's own column is a closed-form induction that deletes the loop outright
(-0.211 ns and -0.001 ns) rather than an unroll - so unrolling cannot be
measured against that ceiling and would be judged against a transform it is not.
STORE-LOAD is serially dependent by construction and unrolling cannot touch its
latency. Recorded as a measured non-consumer, not as work skipped.

WHAT IS AND IS NOT BUILT. Built and gated: the instrument
(tools/codegen-loop-inventory.f, its runner, its suite, registered in
test/gate-stdlib-cases.f and test/gate-stdlib-inline-lib.f and observed running
as `PASS: compiler-codegen-loop-inventory`), plus the conditional-branch and
return readers it needed in src/compiler/native/branch.f. NOT built: the hoist
itself. Nothing in src/compiler/native changed except that branch reader, no
module rewrite exists, and every corpus row's bytes are unchanged - 45 of 45
identical, codegen-compare 0 findings, so the chain baseline was deliberately
NOT re-pinned because nothing moved.

The one row this leaf still owes is BIG-CONSTS, with the prediction above
registered in advance so the pass can be judged against a number it did not
choose afterwards. This is an open work item, not a completed transform: a
measurement that names a consumer is the go-ahead for the pass, not a
substitute for it. The load half and the unrolling half are the parts that are
genuinely finished, because their answer is a measured zero and a measured
non-consumer respectively.

TOOL FINDING worth keeping: a backward branch in the LAYOUT is not a back edge.
BYTE-FIND's -1 return block is laid out between the loop's own blocks and
branched to from below, so the first version of the inventory reported two loops
where the routine has one and charged that block's four-instruction literal
chain to a body that never executes it. A loop is a CYCLE, so the span is a loop
only when control entering at the target can arrive back at the branch, and the
body is the intersection of what the header reaches with what reaches the latch.
Both are asserted in the suite against that exact row, and reverting either
rule fails at the named assertion rather than in a report.

UNBLOCKED (2026-08-05, user order): the optimization program proceeds NOW on the chain as it stands — the hard cut continues in parallel and is no longer a prerequisite. Standing acceptance for every optimization lane: name the corpus rows expected to improve BEFORE implementing; show the emitted instruction delta on them; every oracle answer preserved bit-for-bit incl. NaN; report BOTH gaps per touched row (chain-vs-clang closed, chain-vs-own-baseline gained) from tools/codegen-compare.f; re-pin the chain baseline with --update-chain only after the report is read; no regression on untouched rows outside a stated multi-objective trade. New instruction forms (madd/msub, ldp/stp, bitfields, ccmp, NEON) require Rocq rows in formal/Common/Insn.v with enc/wf/roundtrip before the emitter uses them — the CG-02 discipline, applied per-lane not deferred.

MEASURED RE-SCOPE (2026-08-06, instrument merged at 8f67723f): unrolling has NO consumer (measured zero) and invariant loads are a corpus-wide ZERO — both halves CLOSED by measurement. What remains of this leaf: hoist exactly the BIG-CONSTS shape (16 of the 18 real in-loop constant chains live in that one row; prediction PRE-REGISTERED in the inventory suite: body 28->12) — judged against the registered number, never one picked afterward. PRESSURE-LOOP is explicitly NOT this leaf's (its wholly-invariant body relocates pressure without reducing it — remat's row). The other 24 in-loop constants belong to habu-select-the-immediate (cheaper, no register cost). Expectation-setting fact for NEON: clang's big loop wins are closed-form induction and vectorization, not hoisting.
