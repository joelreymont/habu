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
