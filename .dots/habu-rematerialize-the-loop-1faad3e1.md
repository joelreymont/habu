---
title: Rematerialize the loop-invariant loads
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:03:15.160104+02:00"
---

PRESSURE-LOOP road, from the spill lane's measurement: its 14 live values are pure loop-invariant loads off one base, so rematerialising them in place needs no frame slot and no new memory order. Probe this road FIRST (the lane's probe shape at /private/tmp/claude-501/spill-probe-final.f) before any frame work; if remat closes the row, the deep redesign (habu-spill-from-a-4145325c) stays unneeded for the corpus. Purity must come from the IR's own facts, not assumption. Acceptance: PRESSURE-LOOP compiles, answers bit-for-bit, validator extended to check remat correctness (a remat'd value equals the load it replaces — differential test), no other row moves, both-gaps reported, deliberate re-pin.

Consolidation (2026-08-05): shares one remat design and lane with habu-rematerialize-constants-cdce9a24 — constants land first, these loads second on the same machinery.

SHARED-MACHINERY NOTE (2026-08-05, remat). The consolidation block above is
still right that one remat design serves both leaves, but the constants leaf's
premise was corrected before any code was written and this leaf inherits the
correction: see habu-rematerialize-constants-cdce9a24. In short, the allocator
runs on the MACHINE module, so there is no operand-free constant there - a
literal is a tied movz/movk chain that forms one register class - and re-emission
is only cheaper than a spill when the chain is one instruction. The shared
machinery is therefore not "re-emit the defining op" but "re-emit the class's
defining CHAIN, when that costs no more than the reload it replaces", plus the
CL-SLOT split described in that dot (a class can be evicted without holding a
slot, which five sites in regalloc.f currently cannot express).

WHAT THAT MEANS FOR THE LOADS. A pure loop-invariant load is a single
instruction, so on the cost test it is a candidate wherever a constant chain of
length one is. The purity question is untouched and remains this leaf's own: the
IR has to say the load is pure and that its base and memory order are unchanged
across the re-emission site, from its own facts. If it cannot, this leaf stays
refused with the missing fact named, per its acceptance. That question was NOT
investigated in this lane.

NOT BUILT. Nothing in src/ or test/ changed for this leaf.

Claim: unassigned

Claim released 2026-08-05 (remat2): deferred behind the production rewrite loop and the constants leaf, see habu-rematerialize-constants-cdce9a24.

REFUSED 2026-08-12 (lane spill-close, agent spillclose2), WITH THE MISSING FACT
NAMED, which is what this leaf's acceptance asked for. The constants answer does
not extend to the loads by reading a fact off the form, because the fact the form
declares is the opposite one.

WHERE PURITY IS DECLARED, AND WHAT IT SAYS. The move-wide's form carries
PURE-VALUE and TOTAL (src/compiler/native/a64ir.f DEF-MOVZ), and PURE-VALUE is
IR-SCHEMA:SET-PURE, which src/compiler/ir/schema.f defines as "no token, no
domain, no space, no alias". That is the fact re-emission rests on: a move-wide
joins no memory order, so writing it again in a block where this pass cannot
state where an access stands is sound.

THE LOAD FORMS DECLARE A MEMORY READ. DEF-LDR (the frame load) is
IR--SCHEMA-EFFECT:READ FRAME-MEM - space LOCAL, alias UNALIASED - and DEF-ALDR
(the address load, which is what `base @` selects, since the frame form carries a
KEY-SLOT attribute and no address operand) is IR--SCHEMA-EFFECT:READ ADDR-MEM -
space GENERIC, alias UNRESTRICTED. Neither is PURE-VALUE. UNRESTRICTED is the
strongest available statement that the read may alias anything, so the schema of
PRESSURE-LOOP's own loads says nothing in the form licenses reading the same
address again later.

AND THE SHAPE SAYS IT TWICE. A load form takes the memory token as an operand
and answers a NEW one (src/compiler/native/spill.f EMIT-LOAD threads both), so a
re-emitted load mints an order every later reader would have to be re-threaded
onto. That is a frame access in a middle block - exactly what KEEP? refuses, and
exactly the refusal a re-emission is allowed to override only BECAUSE it touches
no token.

MEASURED, NOT ONLY READ. Deleting the opcode test in MB-DEF-OP? makes the
fourteen-load body a candidate: the walk decides one re-emission and the run dies
at E-IR-VERIFY-ATTRKEY (-8095), because what is emitted is a move-wide carrying a
load's own attributes. The module's own verifier refuses it.

THE MISSING FACT, NAMED. "The alias class this load reads is unwritten between
the definition and the point the re-emission would stand at." It is derivable
from the module - the token chain says who writes, and the schema says each
writer's space and alias class - but it is DECLARED nowhere, and no pass in this
chain derives it. Building it is a memory-dependence analysis, not an eligibility
clause; and even with it, a re-emitted load still joins the memory order, so it
needs the second capability too: placing a memory access in a block that is
neither the entry nor the exit, which is the KEEP? capability itself.

SO PRESSURE-LOOP IS NOT MOVED BY THIS ANSWER and the judge's refused count is
unchanged at 1. The next step is one of two dots, not this one: a loop-invariant
load hoist with its own dependence proof, or the KEEP? capability. Nothing in
src/ or test/ changed for this leaf.
