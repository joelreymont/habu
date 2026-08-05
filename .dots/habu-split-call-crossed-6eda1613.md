---
title: Split call-crossed values around the loop
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T17:03:15.149470+02:00"
---

CALL-PRESSURE road, from the spill lane's measurement (2026-08-05, bookmark spill): a value live across a CALLLESS loop already spills today (probe: 7 live across a callless loop compiles rc 0; the same 7 across a call in the loop refuses E-A64RA-SPILL -8508) — the refusal comes from MB-KEEP-BLOCK KEEPing every operand of the call site's data-stack saves in the middle block, NOT from loop residency. Fix: split the live range so the value is dead across the loop — store in the entry block, reload in the exit block — a placement ORDER-CK already permits; no middle-block frame redesign (that stays with habu-spill-from-a-4145325c and is NOT needed here). Also: correct tools/codegen-compare-new4.f's stale 'one refusal, two roads' paragraph, and promote the lane's probe (/private/tmp/claude-501/spill-probe-final.f) into tools/ as the regression instrument. Acceptance: CALL-PRESSURE compiles, answers match the engine bit-for-bit, the A64RAV validator checks the split slots, no previously-supported row changes bytes, both-gaps reported, corpus-4 baseline re-pinned deliberately.

Claim: agent=callsplit3 workspace=.jj-ws/habu-split-call-crossed-6eda1613

MEASURED 2026-08-05 (agent=callsplit). STOPPED BEFORE THE FIX, with the dot's
headline finding CONFIRMED and its stated mechanism and prescribed fix both
FALSIFIED BY MUTATION. No compiler code changed. What landed is the measurement
instrument; what did not land is the split, and the reason is below.

WHAT IS CONFIRMED. CALL-PRESSURE's refusal is not loop residency. Seven values
live ACROSS a callless loop compile (rc 0). That was the predecessor's finding
and it reproduces exactly.

WHAT IS NEW, AND IT NARROWS THE CAUSE FURTHER. Two more controls isolate it:

  - Seven locals live across the SAME call with NO loop around it compile
    (rc 0). So the call alone is not the wall either.
  - The deciding pair: the same seven locals, the same loop, the same call, the
    same eighteen registers - read BEFORE the loop they compile (rc 0), read
    AFTER it they are refused (-8508). Nothing else differs.

Reading a local after a call is exactly what marks it as one that must survive a
call (src/compiler/native/elaborate.f CROSS-STEP:1754). So the crossing is the
whole of it.

WHAT IS FALSIFIED, AND THIS IS THE PART THAT CHANGES THE WORK. The dot says the
refusal comes from MB-KEEP-BLOCK keeping every operand of the call site's
data-stack saves in the middle block, and prescribes a fix that is "a placement
the allocator and ORDER-CK already permit". Mutation of the real allocator says
otherwise. Each mutation was applied to src/compiler/native/regalloc.f, measured
through the real migration entry, and reverted; the file is byte-identical to
756c7d06 now.

  - Cut the middle block's rule down to the entry block's (keep only data-stack
    reads): CALL-PRESSURE still refuses -8508. PRESSURE-LOOP moves -8508 ->
    -8329, so the rule is load-bearing for THAT row and not for this one.
  - Remove the block-argument marking entirely: CALL-PRESSURE still refuses
    -8508.
  - Relax the multi-value class exclusion alone: still -8508.
  - Relax the KEEP test alone: still -8508.
  - Relax the KEEP test AND the class-size test together: the refusal finally
    moves, to -8335 E-A64RAV-REGISTER - the independent validator rejecting the
    allocation, which is the validator doing its job.

The throw site was identified rather than guessed: the three E-A64RA-SPILL sites
were given distinct temporary codes, and both CALL-PRESSURE and PRESSURE-LOOP
reach MB-VICTIM's nothing-spillable path, not MB-PLAN-TAIL-CK.

So the classes holding registers at the failing position are excluded for
DIFFERENT reasons - some kept, some tied by an edge into a class of more than one
value - and lifting any single exclusion leaves every candidate still excluded by
another. That is why no single-cause fix moves this row, and it is why a
placement change cannot: both exclusions are CREATED by the crossing, upstream of
the allocator. A crossing local is threaded through the loop as a block argument
of every block on the path (LOCAL-ARGS+) and as an operand and result of the call
itself (CALL-OPERANDS+), and those are what tie it into a multi-value class and
mark it kept.

WHAT THE REAL FIX IS, AND WHY IT WAS NOT BUILT HERE. To make the value dead
across the loop it has to stop being threaded - parked in a frame slot before the
loop and reloaded after, removed from the block-argument lists of every block on
the path and from the call's operand and result lists. That is an ELABORATOR
capability, not an allocator placement, and it changes the call-crossing protocol
whose own source says what a mistake costs: "A value left off the operand list is
a value that stays in a register the callee's contract declares destroyed, and it
comes back wrong" (elaborate.f:2223). It also sits much closer to
habu-spill-from-a-4145325c than this dot assumed when it ruled that dot out of
bounds. Building it on a mistaken mechanism, or half-building it, is how a silent
miscompilation lands, so it stops here for a re-cut rather than being attempted
against a description the tree contradicts.

WHAT LANDED. tools/codegen-spill-probe.f, the predecessor's scratch probe
promoted and made an assertion instead of a printout, registered in the
codegen-compare suite in test/gate-stdlib-cases.f. It pins eight facts: both
walls, both controls, the two isolating cases (callless loop, loopless call) and
the deciding before/after pair. One defect in the inherited probe was found and
fixed: its straight-line case dropped the call's result and so failed to certify
(rc 70) - it was measuring nothing, and it is the case that turns out to matter
most, because it is what rules the call out as the cause on its own. The header
records the mutation matrix above so the next reader does not re-derive it.

WHAT WAS NOT DONE, DELIBERATELY. The split itself. The corpus-4 chain baseline
was NOT re-pinned - CALL-PRESSURE still refuses, so there is nothing new to pin
and re-pinning would record a gap as if it were a result. The stale "one refusal,
two roads" paragraph in tools/codegen-compare-new4.f was NOT rewritten: it is
wrong in the way this dot says, but the correct replacement depends on which fix
is taken, and a second wrong paragraph is worse than a stale one. PRESSURE-LOOP's
row and its gap line are untouched, as instructed.

Is this the best long-term answer or a patch? It is a refusal to implement
against a falsified mechanism, plus the instrument that falsified it. The
mutation matrix is the evidence and it is reproducible from the tree; the fix
needs a re-cut leaf that names the elaborator's crossing protocol as the owner
and settles the overlap with habu-spill-from-a-4145325c first.

SUITE PLACEMENT, learnt the hard way and recorded so the next reader does not
repeat it. The probe was first listed as a member of SUITE codegen-compare.
Suite members share ONE process, and the probe requires the fourth corpus's
cases to reach a callee - so the comparison then measured corpus 4 against
corpus 3's committed baseline and reported 115 findings (MISSING-ROW for every
corpus-4 row, EXTRA-ROW for every corpus-3 row). Passing by hand proved nothing
about that, because by hand it is the only thing in its process. It now has its
own SUITE and the gate schedules it (PASS: codegen-spill-probe).

RED SET, MEASURED ON THE REBASED TREE (master ee6463fc). The gate's
codegen-compare member fails on a nested phase named `refuse`, whose forked
reference worker throws -8264 E-CODEGEN-COMPARE-STAGE. It is NOT this lane's:
the three files that member loads (tools/codegen-compare-test.f,
tools/codegen-compare-clang-test.f, test/codegen-fork-reference-test.f) are
untouched here, and running exactly those three directly reproduces the failure
on 756c7d06 AND on ee6463fc. The comparison's own measurement is clean in both
(codegen-compare: 0 finding(s)). This is a red beyond the four the dispatch
listed as known, and it wants its own dot.

RE-CUT (2026-08-05, after the mutation matrix in tools/codegen-spill-probe.f falsified the mechanism above): the wall is created UPSTREAM of the allocator. elaborate.f CROSS-STEP marks a local read after a call as call-surviving; LOCAL-ARGS+ then threads it as a block argument of every block on the loop path AND CALL-OPERANDS+ makes it an operand and result of the call — both exclusions from MB-SPILLABLE? originate there, and no single allocator-side relaxation moves the refusal (five mutations measured; relaxing two together produces code A64RAV correctly refuses). The fix is therefore an ELABORATOR change to the call-crossing protocol: split the value around the loop AT ELABORATION — store to its slot before the loop, do not thread it through the loop blocks or the call's operand list, reload after the exit — while honoring the contract elaborate.f:2223 states (a value left off the operand list must not stay in a register the callee destroys; here it lives in memory across the whole region, which satisfies it). Coordinate with habu-spill-from-a-4145325c BEFORE implementing: this sits close to its territory, and whichever leaf owns the elaborator protocol change must be the only one that does. tools/codegen-compare-new4.f's stale 'one refusal, two roads' paragraph is corrected as part of whichever fix lands (its correct replacement depends on the fix's shape).


Ownership reconciliation (2026-08-05, agent=callsplit): this leaf owns the elaborator-side call-crossing protocol change, and habu-spill-from-a-4145325c keeps only the allocator-side middle-block frame-order redesign, which this fix neither touches nor needs.

DESIGN MEASUREMENT (2026-08-05, agent=callsplit), before implementing the RE-CUT.
Two facts about the tree change the shape of the fix, and both were checked
rather than assumed. Reconciliation with habu-spill-from-a-4145325c is done and
cross-referenced in both leaves.

ONE. THE ELABORATOR CANNOT "STORE TO ITS SLOT". There is no frame at that level
to store into. elaborate.f says so at its own head - "THE UNIT IS THE DEFINITION,
AND THAT IS WHY THERE IS NO FRAME TO FIND" - and every FRAME/SLOT token in that
file is the counted-loop control frame (DO-FRAME) or a data-stack slot, never a
machine frame slot. A spill is not an IR operation at all: the allocator records
it as a PLAN (regalloc.f P-STORE / P-RELOAD, lines 223-224, placed by
MB-PLAN-STORES / MB-PLAN-LOADS), and src/compiler/native/spill.f is what reads
that plan. The frame's layout has exactly two owners today
(src/compiler/native/frame.f: the prologue's link slot, then the allocator's
slots in the order it hands them out), and that file exists BECAUSE two passes
once placed things in the frame without reading each other.

So the split cannot be "elaborator stores, elaborator reloads" without either a
new dialect operation for a frame access or a third owner of the frame. The
smaller and better-shaped form is a DIRECTIVE rather than an emission: the
elaborator stops threading the value (out of LOCAL-ARGS+, out of CALL-OPERANDS+,
out of LOCAL-RESULTS@) and marks it must-spill; the allocator, which already owns
NEW-SLOT, CL-SLOT and the store/reload planning, places it unconditionally
instead of only under pressure. Every piece of machinery that needs to exist
already does, in the pass that owns the frame.

THE HAZARD THIS MUST CLEAR, AND IT IS THE ONE elaborate.f:2223 NAMES. Removing a
value from the call's operand list is only safe if something else guarantees it
is NOT in a register across the call. Nothing does that today: the allocator
spills under pressure, at its discretion, and a value it chose to keep in a
register would be destroyed by the callee with no diagnostic anywhere. That is
why the mark has to make the spill MANDATORY, and why A64RAV has to check the
slot exists rather than check that the threading is gone - the absence of
threading is the dangerous half, and the slot is what makes it safe.

TWO. "ACTIVATE ABOVE PRESSURE" CANNOT BE DECIDED WHERE THE SPLIT IS MADE.
Register pressure is the allocator's discovery - MB-FIT finds it by scanning and
re-scanning - and the elaborator runs two passes earlier. Any pressure test
written into the elaborator would be an estimate, which the Fix Review Gate
rejects on its face: a value heuristic standing where a structural fact is
available. The structural trigger is a RETRY. Elaborate and allocate exactly as
today; only when the allocation refuses with E-A64RA-SPILL, re-elaborate the same
definition with the split enabled and run the chain again. Below-pressure bodies
then emit byte-identical code BY CONSTRUCTION, because they never take the second
pass - which is the acceptance constraint met exactly rather than approximated,
and it needs no test to discover what a heuristic would have got wrong. The retry
belongs in migrate.f WORK, which already rebuilds a module per run and already
catches and rethrows the chain's refusals.

WHAT THIS MEANS FOR THE LEAF. The work is four coordinated changes - elaborator
threading suppression under a flag, an allocator mandatory-spill mark, a retry in
the migration entry, and the A64RAV checks - across the pass boundary whose
failure mode is silent wrong answers rather than a refusal. It is a bigger leaf
than "split at elaboration" reads, and the RE-CUT's own wording ("store to its
slot before the loop") is not available as written. Recorded here so the next
lane starts from the tree's shape rather than re-deriving it.

BUILD MEASUREMENT (2026-08-05, agent=callsplit2), against the DESIGN CORRECTION.
Four of its statements were checked by building them. One holds and is much
smaller than written; two are wrong about the tree; one is a shape the tree
refuses. No compiler code is committed - src/compiler/native/elaborate.f and
migrate.f are byte-identical to bd7832fc - because a half-built version of this
is the silent miscompilation the leaf exists to avoid.

ONE, AND IT HOLDS. THE DIRECTIVE IS A SINGLE LINE. Every carrier of a crossing
local asks elaborate.f CROSS-L (:909) for its count - the block arguments
(LOCAL-ARGS+ via OPEN-ARGS :1171, TERM-BR :1248, STUB :1275, the loop head :2148
and exit :2181), the call's operands (CALL-OPERANDS+ :2287), its result count
(CROSS-RESULTS :2297) and its take-back (LOCAL-RESULTS@ :2342) - because the list
is all of them or none and LOCAL-CK (:951) refuses anything between. So answering
nought under a flag IS "stop threading", stated once. MEASURED through the real
chain: with the flag off the probe's POST7 shape refuses -8508 as today; with it
on the same source compiles rc 0. Nothing else in either file had to change.

TWO. THE MUST-SPILL MARK CANNOT CROSS THE PASS BOUNDARY AS WRITTEN. The allocator
never sees a HIR value. src/compiler/native/select.f rebinds every one of them to
a fresh a64 value through VMAP (VBIND/VOF :579-588, called at some twenty sites),
so an elaborator-side per-value mark reaches the allocator only if select.f
translates it - a fifth coordinated change, in the pass the correction does not
name.

THREE, AND THIS IS THE ONE THAT CHANGES THE WORK: THE MARK IS NOT NEEDED, BECAUSE
THE HAZARD elaborate.f:2223 NAMES IS ALREADY ENFORCED, STRUCTURALLY, FOR EVERY
VALUE. The correction says "nothing does that today: the allocator spills under
pressure, at its discretion, and a value it chose to keep in a register would be
destroyed by the callee with no diagnostic anywhere." The tree says otherwise in
two independent places:

  - regalloc.f MB-CROSSES? (:1563) and MB-FORBID (:1572) walk every position,
    and at each CALL operation a class genuinely spans - a member defined before
    the branch and last read after it - bar that callee's whole destroyed set
    from the class's register. It is asked of every class, not of the call's
    operand list. Its own prose already names this exact shape: "A class live
    from before a loop to after it crosses every call inside that loop, which is
    exactly the shape a local read after a loop of calls has" (:1548-1550).
  - regalloc-verify.f (:523-538) re-derives the same rule independently, off the
    callee's address and its clobber record rather than off the selector, and for
    a callee with no record bars the entire pool - "nothing may cross the call in
    a register at all".

MEASURED: with the threading suppressed and NO mark and NO mandatory spill,
CALL-PRESSURE's body compiles and its answers match the engine bit-for-bit on all
three pinned rows (eight turns, length zero, all-negative), and a one-crossing-
local variant matches on 5, MAX-INT and MIN-INT. So pieces (2) and (4) are not
this leaf's work: forcing a frame round-trip where a non-destroyed register
already serves would be strictly worse code for no safety gained, and A64RAV
already owns the check the correction asks it to grow.

THE DISPATCHED MUTATION FIRES ITS OWN STOP RULE, and it is recorded rather than
worked around. "Remove the MUST-SPILL mark while keeping the threading
suppression, and show A64RAV refuses; if it stays green, STOP." It stays green.
The reading is not that the validator check is wrong - it is that the premise
that A64RAV needs a new check is wrong.

FOUR. THE RETRY IS NOT A RE-RUN OF THE TAIL OF WORK. migrate.f MODEL (:314) mints
the word model against `CC BB IR-BUILD:MODULE-KEY`, and NELAB:COLON checks every
identity read off the tape against the builder's module - so a second attempt
needs a second builder AND a second model, not just a second builder. Moving
MODEL inside the retried part puts it after the publication prologue, and every
migration then throws E-IR-BUILD-STALE (-8060), including ones that never retry -
the ordinary probe suite goes red. Which of RECORD / PUBLISHED-ONE /
RESOLVES-TO-LATEST / KEEP-NAME depends on the live builder is the next lane's
first measurement, and it is a measurement and not a reading.

WHAT THE LEAF LOOKS LIKE NOW. Piece (1) is one line and is proven. Piece (3) is
the open problem and it is a migrate.f lifecycle question, not an allocator one.
Pieces (2) and (4) should be struck unless the next lane can produce a body where
MB-FORBID and A64RAV together do NOT already cover the un-threaded value - which
is the one experiment that would reinstate them, and it is worth running before
the pieces are dropped for good.

DESIGN CORRECTION (2026-08-05, measured against the tree; supersedes the RE-CUT's 'store to its slot before the loop' wording — the elaborator HAS no frame, by design: frame.f has exactly two owners): the split is a DIRECTIVE plus a RETRY. (1) Elaborator: under a flag, stop threading the call-surviving local (out of LOCAL-ARGS+, CALL-OPERANDS+, LOCAL-RESULTS@) and mark the value MUST-SPILL. (2) Allocator: a must-spill mark makes slot placement unconditional (NEW-SLOT/CL-SLOT + P-STORE/P-RELOAD planning as today) — mandatory, because the un-threaded value absent from the call operand list is safe ONLY if it provably lives in memory across the call (elaborate.f:2223's hazard; the failure mode of getting this wrong is silent wrong answers). (3) Trigger: structural, not estimated — migrate.f WORK retries once: elaborate/allocate exactly as today, and only on E-A64RA-SPILL re-elaborate with the split enabled and re-run the chain; below-pressure bodies are byte-identical BY CONSTRUCTION (they never take the second pass). (4) A64RAV checks the SLOT EXISTS and is stored before the region and reloaded after — the slot is what makes the missing threading safe, so the validator's primary object is the slot, not the absence. Probe suite tools/codegen-spill-probe.f is the regression floor. The rejected alternative branch (a dialect frame-access op so elaboration could emit stores directly) adds a cross-pass primitive with no second consumer — do not build it.

GENERATION 4 (2026-08-05, measured; supersedes pieces 2 and 4 of the DESIGN CORRECTION): (1) the elaborator directive is ONE LINE and proven — every carrier asks CROSS-L for its count and the list is all-or-none (LOCAL-CK), so answering nought under the flag is the whole suppression; flag on, CALL-PRESSURE's shape compiles rc 0 through NMIGRATE:DEFINE-CALLING with answers bit-identical to the engine on all pinned rows plus MAX-INT/MIN-INT variants. (2)+(4) STRUCK: the 2223 hazard is already enforced twice — regalloc.f MB-CROSSES?/MB-FORBID bars each spanning class from every callee-destroyed register (asked of every class, not the operand list), and regalloc-verify.f:523-538 re-derives the rule independently off the callee's clobber record, barring the whole pool for a record-less callee. A mandatory spill would force a frame round-trip where a non-destroyed register already serves — worse code, no safety. Reinstatement experiment before striking forever: find a body where MB-FORBID plus A64RAV do NOT cover an un-threaded value (none found; none proven absent). (3) REMAINING WORK, the only unbuilt piece: the retry. migrate.f MODEL (:314) mints against CC BB IR-BUILD:MODULE-KEY and NELAB:COLON checks tape identity against the builder's module, so a second attempt needs a second builder AND model; naively moving MODEL after the publication prologue makes EVERY migration throw E-IR-BUILD-STALE. The retry must be a full second WORK-shaped pass — answer first: what in the publication prologue depends on the live builder. select.f's VMAP rebinding (:579-588) is the general obstacle to any elaborator-to-allocator per-value mark, recorded for future passes.

GENERATION 5 (2026-08-05, agent=callsplit3): the retry's prologue question,
ANSWERED, and the answer rules out the dispatched shape. No compiler code
changed; migrate.f is byte-identical to master.

WHAT IN THE PUBLICATION PROLOGUE DEPENDS ON THE LIVE BUILDER: exactly one word,
and it is worse than a dependency. migrate.f RECORD (:337) is the only one -
PUBLISHED-ONE, LATEST-WID, LATEST-NAME$, RESOLVES-TO-LATEST and KEEP-NAME touch
neither BB nor CC nor MKEY, they read the engine dictionary. RECORD does three
things bound to one unit: it mints the TAPE against CC BB IR-BUILD:MODULE-KEY,
it opens the feed unit, and it runs SCAN - which is `SRC$ EV`, the ENGINE
COMPILING AND PUBLISHING THE DEFINITION. PUBLISHED-ONE then asserts that exactly
one dictionary record appeared.

SO THE RETRY IS NOT "RE-RUN ELABORATE-THROUGH-EMIT". Attempt two needs a tape,
the tape is keyed to a module, a module belongs to a builder, and the tape is
made only by evaluating the source. Re-running from RECORD therefore evaluates
the source a SECOND time and publishes the word a SECOND time; not re-running it
leaves attempt two with a tape NELAB:COLON must refuse as another module's.

THE THREE SHAPES, AND WHY EACH IS BIGGER THAN THE DISPATCH SAYS.

  (A) SECOND BUILDER, SECOND MODEL, SECOND TAPE - the dispatched shape. It
      requires a second RECORD, hence a second `SRC$ EV`, hence a duplicate
      dictionary record. Habu forbids silent redefinition, so it needs the first
      record retired (XREF-RETIRE, src/habu/xref.f:360) before re-evaluating -
      which is available, but makes the migration compile every retried body
      twice in the engine and leave a retired record behind per retry. That is a
      dictionary-lifecycle change, not a migrate.f-local one.

  (B) ONE BUILDER, ONE TAPE, TWO FUNCTIONS - elaborate the same tape twice into
      the same module and allocate the second function. It needs no second tape,
      model, RECORD or publication, which is why it fits the tree's grain best.
      It is refused in FOUR places: regalloc.f:1931, regalloc-verify.f:251,
      spill.f:920 and emit.f:1720 each throw *-SHAPE unless FUN-COUNT is exactly
      one. IR-BUILD:ABANDON-FUN (build.f:927) abandons a function being STAGED,
      not a completed one, so the refused first function cannot simply be
      dropped: NELAB:COLON ends with END-FUN.

  (C) MAKE A COMPLETED FUNCTION ABANDONABLE - a transactional rewind at the
      builder, so attempt one leaves the module as it found it. This is what the
      dispatch assumed CG-07 had landed; it has not. The only REWIND in the tree
      is TFAM-REWIND in the checker's type-family registry (src/core/checker.f,
      type-family.f, sumtype.f), which is a different registry and not a builder
      facility.

WHAT THE NEXT LANE SHOULD DECIDE FIRST, because it is a design choice and not a
measurement: whether the retry pays a second engine compile plus a retired
dictionary record (A), or the four one-function shape rules are relaxed to
"allocate the LAST function" and a completed function becomes abandonable (B/C).
(B) with (C) is the smaller runtime cost and the larger blast radius; (A) is
migrate.f-plus-dictionary and touches no pass. Neither is a twenty-line change,
and both fail in ways that corrupt the dictionary or leak a module, so neither
should be attempted without deciding this first.

STILL TRUE AND STILL UNBUILT: piece (1), one line in elaborate.f CROSS-L, proven
in generation 4. It has no consumer until the retry exists, so it stays uncommitted.

DECISION (2026-08-05, orchestrator, generation 5): the retry is PARKED behind the hard cut. The generation-5 measurement showed evaluation and publication are fused in the engine's EV (migrate.f RECORD: the tape exists only by evaluating the source, and evaluating publishes), so a bridge-side retry costs either a second engine compile plus a retired record per retried body (shape A) or four relaxed FUN-COUNT shape rules plus new completed-function-abandon builder machinery (shapes B+C) — both are investments in NMIGRATE, which habu-delete-the-old-679cfd35 deletes at the cut. The retry therefore belongs to the SOLE-COMPILER pipeline (habu-cut-colon-compilation-a5aa3f1f now carries the requirement), designed against a compile path where elaboration is re-runnable without dictionary effects. Until then: the one-line CROSS-L directive stays uncommitted (no consumer), CALL-PRESSURE keeps its honest gap line, and this leaf holds five generations of evidence for whoever builds the final retry. Prerequisite noted: CG-07's transactional builder had NOT landed when generation 5 checked (retire-ir lane in flight) — the final retry leans on it.
