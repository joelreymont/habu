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

Stage-3 monolithic claim released (agent=derive3 stopped with evidence, no edits - the mapped seams failed against source; findings and the re-plan below).

2026-07-20 STAGE-3 REPLAN (derive3 lane's Dig-Protocol findings + orchestrator decision):

TWO SEAM FAILURES, both structural:
(1) NO INPUT-REDIRECTION PRIMITIVE: the frozen engine exposes only parse-name
    (destructive) and evaluate - no >IN/source/save-input. Tables filled DURING
    PARSE-SIG (CAP-IN-AT, MI-IS-*, NT names) cannot bind to a derived count
    without first consuming the tokens. The mapped "signature pre-scan" requires
    a parser refactor: capture the whole MODEL:...; line into a scratch buffer
    via a parse-name loop, then re-tokenize maki-side (~6 parse-name call sites
    move off the input stream). This also gives MSRC sizing from the captured
    line for free.
(2) THE MIR TABLE IS TWO-PHASE: backward.f appends adjoint nodes via 14 MIR-OP+
    sites AFTER CAP-FINISH (2-block: 32 fwd -> 133 total), and LDEFER-BIND's
    grow ABANDONS the predecessor region - growing at BW-BUILD would discard the
    forward nodes. The stage-2 mapping overlooked backward-phase growth.

DECIDED (orchestrator, long-term-best): (a) COPY-ON-GROW in the engine definer -
a grow variant that preserves live cells (memmove to the fresh region before
abandoning). Rationale: it is the GENERAL mechanism (any two-phase table gets it
free), needs no adjoint fan-out estimate table coupled to every BW-STEP emitter
(option b's drift risk), no fwd/bwd region id-split arithmetic (option c), and
mid-build base motion is SAFE BY CONSTRUCTION because deferred accessors read
the offset cell per call - the audit is only that no raw accessor-derived
pointer is held across an append. With copy-on-grow, MI-* binds at CAP-FINISH
to the forward count and grows incrementally during BW-BUILD - no exact adjoint
pre-count needed anywhere.

CLEAN FAMILIES (verified single-phase, convertible now): executor EX-OFF at
EX-PLAN (the stage-1 pattern) + EX-IN-PTR/EX-IN-SET at EX-RESET from
SLOT-COUNT@; backward BW-CT-*/BW-ISG-* at BW-BUILD entry from
NODE-COUNT@/SLOT-COUNT@; tensor-value P-*/TV-* by count-during-parse (CAP-OPS +
a transpose counter; P-INS by per-op-arity accumulation, under-count loud-fails
on the DEFER bounds check) - the TV part waits on the parser refactor only if
the counter needs the captured buffer (verify; CAP-OPS may suffice pre-refactor).

12-BLOCK MATH CONFIRMED ALL-OR-NOTHING: every cap blocks at 12 blocks (172 fwd /
723 total / 164 inputs vs CAP-CAP 64, NT-CAP 96, MIR-IN-CAP 64, PLAN-CAP 64,
PLAN-INCAP 256, TV-CAP 256, BW-NCAP 128, BW-SCAP 64, EX-IN-CAP 64, MSRC 2048);
only MIR-CAP/EX-NCAP 1024 have headroom.

DECOMPOSITION (each lands gate-green independently):
  (i)  engine: LDEFER copy-on-grow variant + red-first + CODELEN rows;
  (ii) parser: MODEL: line buffer-capture + maki tokenizer (cad.f refactor);
  (iii) clean families: executor + backward conversions (disjoint files);
  (iv) capture-side: CAP/NT/MSRC/TV/P conversions (after ii);
  (v)  model-ir MI-* via copy-on-grow (after i, iv) + the 12-block acceptance +
       ceiling-pin conversions.

Stage-3i LANDED (ldgrow lane, f0bbf055): see below.
Stage-3ii LANDED (capbuf lane): see below.
Stage-3iii LANDED (cleanfam lane, aeef6895): see below.

2026-07-20 STAGE 3(ii) LANDED (Buffer-capture MODEL: definition before parsing):
MODEL: now captures the entire definition token-stream into CAPSRC-BUF via ONE
parse-name loop, then all parsing (PARSE-SIG/PARSE-BODY/named forms) re-tokenizes
the buffer with the maki-side NEXT-TOK cursor - the two-pass (count then fill)
capability stage 3(iv) needs, with zero engine change. Behavior-neutral proven:
all 307 in-tree MODEL: defs parse identically (bodies verified plain-token-only,
first ; terminates, trailing-token behavior pinned); overflow die red-first
(CAPSRC-CAP 1024, largest in-tree def 105 B, becomes derived later); timing +13ms
on cad-test (~0.9%), noise elsewhere. BIND-SHAPES' BS-PARSE is a separate
command left untouched.

2026-07-20 STAGE 3(i) LANDED (f0bbf055): LDEFER-GROW copy-on-grow binder +
per-column NAME-GROW word. Settled from source: NEW word beside NAME-BIND
(preservation is explicit opt-in; BIND semantics untouched, stage-2's 19 cases
green unmodified); unbound grow dies E-LAYOUT-UNBOUND (grow-before-bind is a
caller bug, loud); doubling lives in the engine (grow-to-at-least max(2*cap,
count), clamped at the ceiling so it never over-allots); transactional
E-LAYOUT-CEIL before any mutation; within-capacity regrow zeroes exposed slots
(shrink cannot leak dirty cells). USAGE LAW in the header: accessors re-read the
offset cell per call so base motion between calls is safe - the one hazard is a
raw accessor-derived pointer held across a grow. Per-guard falsification proofs;
CODELEN neutral; census rows reconciled at merge (using-import landed between
base and merge: 3546 -> 3548 with the two new defs; AXR ledger renumbered around
the checker-using insertion).

REMAINING: stage 3(iii) in flight (executor/backward clean families); then
3(iv) capture-side counting (the two-pass NEXT-TOK walk over CAPSRC) + CAP/NT/
MSRC/TV/P conversions; then 3(v) MI-* onto BIND-at-CAP-FINISH + GROW-during-
BW-BUILD + the 12-block acceptance + ceiling-pin conversions.

2026-07-20 STAGE 3(iii) LANDED (aeef6895): executor + backward families derive-
sized. Executor EX-OFF bound at EX-PLAN from the run's node count AND at EX-OFF!
from NODE-COUNT@ for the checkpoint path (which never calls EX-PLAN - a seam the
mapping missed, found by the lane); EX-IN-PTR/EX-IN-SET bound at EX-RESET from
SLOT-COUNT@. Backward typed columns BW-CT-AT/BW-ISG-AT converted to
DEFER-LAYOUT-BUFFER (operand-ref verified arity-0 nominal - the class stage-2
proved); raw flag siblings hand-deferred in lockstep; BW-ISG binds
SLOT-COUNT@+1 preserving the seed-slot slack. Old caps became generous ceilings
(E-EX-CAP/E-BW-CAP named, transactional - prior live counts intact, fresh build
succeeds after a die). Both-size regression asserts exact live counts for two
models in one image. Implementation law recorded: variable-fetched bases lose
the ptr element type - all indexed access goes through typed T-AT (base-first),
never bare +. Training locks bit-identical; timing flat (1.38/1.53s suites).

REMAINING: 3(iv) capture-side counting + CAP/NT/MSRC/TV/P conversions (the
two-pass NEXT-TOK walk is landed and waiting); 3(v) MI-* via BIND-at-CAP-FINISH
+ GROW-during-BW-BUILD (both landed and waiting) + the 12-block acceptance +
ceiling-pin conversions. All prerequisites are now in - 3(iv)+3(v) can be ONE
lane owning cad.f/model-ir.f/tensor-value.f end to end.

Stage-3iv+v claim: agent=derive45 workspace=.jj-ws/fable-derive45 machine=spark (owns maki/cad.f model-ir.f tensor-value.f + gptblock-attn-test.f + capacity regressions - the final conversion wave; all prerequisites landed: NEXT-TOK two-pass c4db8303, DEFER columns 4f45b5d1, NAME-GROW f0bbf055, clean families aeef6895)
