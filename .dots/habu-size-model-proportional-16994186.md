---
title: Size model-proportional tables from the model, not constants
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T16:01:34.666729+02:00\""
---

Joel-directed 2026-07-20, design settled after two rounds of his review (first correction: not runtime - load time; second: is declaration-first really best? No). FINAL HIERARCHY:
(1) library constants (today) - worst: recompile the framework to scale;
(2) user pre-declaration - second-best: leaks internal units (the user must know blocks-to-IR-nodes expansion, a formula that changes whenever adjoint derivation changes) and adds load-order brittleness;
(3) DERIVE FROM THE MODEL AT ITS LOAD - the design this dot now specifies.

PRIMARY DESIGN - derive-from-model, all at load time:
1. The capture pass already walks every op when a MODEL: body loads - it COUNTS nodes; the backward builder knows the adjoint expansion; the executor arena need is derivable from shapes. No user declaration.
2. At model BUILD (still load time) the tables are allotted at exactly the counted sizes - plain linear allot in the DATA region, no heap. Capture's own buffers are bounded by the loading file's size, knowable up front.
3. Failure boundary: one place - an absurd model dies NAMED at build against a generous sanity ceiling; nothing partially constructed (transactional, the MIR-LN-CK bar).
4. Multi-model images (the test suite builds 169 suites' models sequentially): mark-and-release arena discipline around each model build (the classic HERE-rewind idiom, scoped to a dedicated model arena so unrelated allotments never interleave), or measured grow-to-largest reuse - decide with evidence, leak bounded either way.
5. THE MEASURED COST, stated up front: column accessor words today compile the table base as an immediate address (LAYOUT-BUFFER defines at library load). Deriving at build means base-bound-at-build - accessors read one cell of indirection. This MUST be measured (executor + checker throughput on the landed gptblock-attn suite, before/after); if measurable, the JIT specializes the base once bound. Do not assume it away; do not accept an unmeasured regression.
6. The declaration word survives ONLY as an optional sanity-ceiling override, never required.
7. Work items: span/base-variable variant of LAYOUT-BUFFER definition with the pointer-rebind audit (everything caching column bases re-derives per build); count-then-allot wiring in capture/build; the arena mark/release; red-first too-big-model die; a regression proving two models of different sizes get exactly-sized tables in one image; snapshot/replay audit (nothing persists raw column addresses).

INTERIM unchanged: the coordinated constant raise (habu-coordinated-capacity-raise-0b4e8a84, in flight) lands first as the labeled interim; this dot is the recorded correct long-term fix. Sequence after that landing.

Claim released (agent=derive landed the maki-side maximum 1758ac9d and stopped at the engine boundary per the escape hatch).

2026-07-20 STAGE 1 LANDED (1758ac9d) + MECHANISM VALIDATED + BOUNDARY MAPPED (derive lane):

LANDED: the executor node-buffer arena is now model-proportional - EX-PLAN sizes it
to the exact need via EX-ARENA-ENSURE (executor.f:455-470), grow-to-largest reuse
past the $8000 seed, EX-ARENA-MAX $80000 sanity ceiling with a transactional named
E-EX-CAP die before any node runs, base read through EX-ARENA-P. Cost: ONE variable
fetch per node-buffer resolution (not per element) - perf leg unchanged. Both
directions proven (34225-cell model dies uncaught on base / runs on branch; ceiling
dies named; state intact after the die).

MECHANISM VALIDATED (the decisive checker finding): the typed-column accessor mint
is authorized by the armed LAYOUT-INTRO window keyed on the pending accessor NAME
and declared signature (checker.f:9004-9007, :1362-1374, :8983-8985), NOT by the
body's arithmetic form - so a rebindable-OFFSET accessor body (data-base <off-cell>
@ + i width* +) type-checks IDENTICALLY to the immediate form. Candidate A
(deferred-offset LAYOUT-BUFFER sibling definer in src/core/layout-buffer.f) is
checker-transparent and the confirmed route; B (maki-side minting) is
forward-reference-unsound; C is the rejected interim.

FULL CAPACITY CASCADE root-caused with minimal repros (12-block = 164 inputs,
~172 fwd / ~723 total nodes): -5024 E-CAD-SYNTAX IS MSRC-CAP 2048 (VERIFIED:
605 B/block marginal, N=3 = 2088 B dies); -5026 E-CAD-INPUTS at CAP-CAP 64
(N>=5); -5045 E-TV-PLAN-FULL is PLAN-CAP 64 op-count (65-op GELU chain repro).
ARCHITECTURAL FINDING: data-space allot ceilings out near 4M cells - production-
scale arenas (GB tensors) eventually need heap allocation, not data-region allot;
the tiny-shape acceptance (~36K cells) is comfortably inside.

REMAINING (the dedicated ENGINE lane, exact scope):
1. src/core/layout-buffer.f: deferred-offset definer (off-cell + count-cell init 0,
   armed accessor reading them, deferred allot, NAME-BIND ( count -- )). CODELEN
   rows same-commit, fixpoint x2 (current rows 136112/944).
2. Rewire typed columns (model-ir.f MI-*, tensor-value.f TV-*/P-*, backward.f BW-*,
   cad.f CAP-*) to the deferred definer; capture scratch sized from the loading
   file's bound; persistent tables from counted nodes.
3. Model arena mark/release + the two-different-sizes regression + pointer-rebind
   and snapshot/replay audits + the 12-block acceptance with pinned accounting.

Stage-2 claim released (agent=lbufdefer landed the keystone 4f45b5d1).

2026-07-20 STAGE 2 LANDED (4f45b5d1): DEFER-LAYOUT-BUFFER in src/core/layout-buffer.f -
three data-base-relative control cells (offset/capacity/live-count, 0 = unbound), armed
accessor reading them (unbound access dies E-LAYOUT-UNBOUND), NAME-BIND with
grow-to-largest reuse mirroring stage 1, transactional E-LAYOUT-CEIL past $100000
cells. THE MECHANISM CLAIM CONFIRMED EMPIRICALLY: the checker accepts the deferred
body identically (armed window keyed on name+signature) - no checker.f edit. 19/19
cases in test/layout-defer.f; fixpoint x2; CODELEN NEUTRAL (boot-loaded source, not
seed image - rows unchanged 136112/944); indirection measured ~3 ns/read (count-cell
bounds check + offset read), per accessor call not per element.

STAGE 3 (remaining, mapped seams from the lane's full capture/build trace):
the 12-block acceptance is ALL-OR-NOTHING across ~13 caps AND the raw create
parallel arrays indexed in lockstep with the typed columns (MI-INCNT/MI-ATTR/
MI-MAT/MI-AD, P-INOFF/P-INCNT/P-ATTR, BW-CT-SET/BW-ISG-SET, NT-NAMES/NT-LENS,
CAP-PEND) - partial conversion is a broken intermediate. Binding seams:
- MIR columns bind at CAP-FINISH entry (after the capture run, before BRIDGE-PLAN)
  from PLAN-N@/P-INS-U/MIR-IS-N;
- P-*/TV-* need count-during-parse (accumulate plan-op count incl. ^T transposes
  + equation arities in PARSE-BODY, bind before CAP-COMPILE-RUN - they fill DURING
  the run);
- BW-* bind at BW-BUILD entry from NODE-COUNT@/SLOT-COUNT@;
- CAP-IN-AT needs a signature pre-scan;
- raw siblings convert to the same deferral (parallel bind) or accessor-routed reads;
- the old caps become the deferred sanity ceiling.

Stage-3 claim: agent=derive3 workspace=.jj-ws/fable-derive3 machine=spark (owns maki/cad.f model-ir.f tensor-value.f backward.f executor.f + gptblock-attn-test.f + capacity regressions; traincore2 lane owns train-core/from-scratch-train/adam-train/eval-train - disjoint; census owns tools+STATUS - disjoint)
