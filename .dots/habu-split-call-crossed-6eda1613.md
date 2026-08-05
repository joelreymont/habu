---
title: Split call-crossed values around the loop
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T17:03:15.149470+02:00"
---

CALL-PRESSURE road, from the spill lane's measurement (2026-08-05, bookmark spill): a value live across a CALLLESS loop already spills today (probe: 7 live across a callless loop compiles rc 0; the same 7 across a call in the loop refuses E-A64RA-SPILL -8508) — the refusal comes from MB-KEEP-BLOCK KEEPing every operand of the call site's data-stack saves in the middle block, NOT from loop residency. Fix: split the live range so the value is dead across the loop — store in the entry block, reload in the exit block — a placement ORDER-CK already permits; no middle-block frame redesign (that stays with habu-spill-from-a-4145325c and is NOT needed here). Also: correct tools/codegen-compare-new4.f's stale 'one refusal, two roads' paragraph, and promote the lane's probe (/private/tmp/claude-501/spill-probe-final.f) into tools/ as the regression instrument. Acceptance: CALL-PRESSURE compiles, answers match the engine bit-for-bit, the A64RAV validator checks the split slots, no previously-supported row changes bytes, both-gaps reported, corpus-4 baseline re-pinned deliberately.

Claim: agent=callsplit workspace=.jj-ws/habu-split-call-crossed-6eda1613

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

DESIGN CORRECTION (2026-08-05, measured against the tree; supersedes the RE-CUT's 'store to its slot before the loop' wording — the elaborator HAS no frame, by design: frame.f has exactly two owners): the split is a DIRECTIVE plus a RETRY. (1) Elaborator: under a flag, stop threading the call-surviving local (out of LOCAL-ARGS+, CALL-OPERANDS+, LOCAL-RESULTS@) and mark the value MUST-SPILL. (2) Allocator: a must-spill mark makes slot placement unconditional (NEW-SLOT/CL-SLOT + P-STORE/P-RELOAD planning as today) — mandatory, because the un-threaded value absent from the call operand list is safe ONLY if it provably lives in memory across the call (elaborate.f:2223's hazard; the failure mode of getting this wrong is silent wrong answers). (3) Trigger: structural, not estimated — migrate.f WORK retries once: elaborate/allocate exactly as today, and only on E-A64RA-SPILL re-elaborate with the split enabled and re-run the chain; below-pressure bodies are byte-identical BY CONSTRUCTION (they never take the second pass). (4) A64RAV checks the SLOT EXISTS and is stored before the region and reloaded after — the slot is what makes the missing threading safe, so the validator's primary object is the slot, not the absence. Probe suite tools/codegen-spill-probe.f is the regression floor. The rejected alternative branch (a dialect frame-access op so elaboration could emit stores directly) adds a cross-pass primitive with no second consumer — do not build it.
