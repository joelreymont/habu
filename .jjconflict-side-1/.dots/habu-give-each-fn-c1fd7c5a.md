---
title: Give each function of a module its own frame
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:52:14.868195+02:00"
---

S1 of the quotations work (habu-compile-a-quotation-04341c80) lands with a named refusal: a quotation body that needs a frame slot (spills, or saves the link because it calls) refuses the migration by name, because the frame is a module singleton - regalloc.f:726-738 keeps one N-SLOTS/BASE-N and one A64RA:FRAME, migrate.f:687 sizes ONE reserve, spill.f:1008 ONCE-CK requires exactly one reserve/release pair. Measured basis for the refusal being acceptable (quotd lane 2026-08-10): all 268 quotation bodies in src+lib are <=13 tokens, 88% a single word call - none spills today. THIS leaf lifts it: N-SLOTS/BASE-N/PRO-N/FRAME/LOWERED's reserve sizing gain a function dimension so a body may spill and call like any routine. Also state the shared VMAX=256 whole-module value budget where E-A64RA-CAP is raised (values are module-wide ids across all functions - TFAM-HOOK-INSTALL's 19 bodies will press on it; decide raise-vs-refuse there with a measurement). Acceptance: the S1 refusal fixture inverts to a publication; a body that spills executes correctly; a body that calls saves/restores its own link; ONCE-CK's invariant restated per function; VMAX pressure measured on the 19-body definition. Files: src/compiler/native/{regalloc,spill,migrate,emit}.f. Depends: habu-compile-a-quotation-04341c80.

TWO NOTES FROM THE S1 LANE 2026-08-10: (1) the spill plan's rows carry
block+position but NOT function; today that is safe by a written
reachability argument (FRAME-ONCE-CK refuses a second function's slots;
abi.f never names a register result) recorded at spill.f HERE? - THE MOMENT
the frame stops being a module singleton, the plan row must carry its
function. That third coordinate is THIS leaf's work. (2) The leaf's text
says S1 refuses a body that calls; the code as built is conservative-correct
instead: CALLS? is a module-wide trait, so every function's entry gets the
PROLOGUE/EPILOGUE when the module calls - a non-calling body pays four
instructions. Reconcile when building: per-function CALLS? is part of the
same per-function frame dimension.
