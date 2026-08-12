---
title: Rematerialize constants instead of spilling them
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:41:33.933692+02:00"
---

Constant rematerialization in the allocator: when an evicted class carries a constant whose materialization chain is length 1, re-emit the movz at use sites instead of store/reload — a cost COMPARISON (re-emission no dearer than the reload), never a blanket rule; a materialised constant at the allocator is a tied movz/movk chain up to 4 long (a64ir.f:251-259) and BIG-CONSTS (corpus 4) is the named regression fixture blanket remat would break.

Grounded design (verified sites): bind MOVZ/MOVK via BIND-DIALECT/SAME-SYM?; split CL-SLOT's double meaning (real slots only) with a per-class remat mark and one shared CL-EVICTED? for the five reader sites (MB-SPILLABLE?/MB-FRAMED?/MB-DUE?/MB-FINISH/MB-PLAN-*); MB-EVICT sets the mark and skips NEW-SLOT (frame.f untouched); MB-PLAN-LOADS plans P-REMAT; spill.f re-emits; the validator re-derives per site.

DEFERRED behind two decisions: (1) the spill rewrite loop must reach production first (fits-or-refuses today — see the cut dot); (2) the validator has no link from a remat site to the class's constant (FLOW-CK ties reload to store via the slot attribute; a wrong-immediate movz is well-formed) — candidates: class identity on the op (weakest — validator would trust the plan), handing VERIFY the pre-spill module (orchestrator's leaning: honest independent re-derivation), or all-movz-to-one-reader-agree. The acceptance's mutation test is impossible until the link exists. Shares one lane with habu-rematerialize-the-loop-1faad3e1 (loads second).

LANDED 2026-08-12 (lane spill-close, agent spillclose2). The re-emission answer
is in the tree: a class holding ONE value whose defining operation is the
move-wide is taken by arranging to write it again in front of each read, with no
slot, no memory token and no place in the order the frame forms thread - which is
why it may override KEEP? where nothing else may.

TWO CORRECTIONS TO THE DESIGN BLOCK ABOVE, both measured while building it.

FOUR READER SITES, NOT FIVE, and the fifth must not move. MB-SPILLABLE?,
MB-FRAMED?, MB-DUE? and MB-FINISH ask "has the walk taken this class's register
away" and read CL-EVICTED?; MB-FINISH asks both questions, in order, because it
has three answers to give. FRAME-ONCE-CK keeps the SLOT and is not a fifth
reader: it is about which function owns the module's one frame, and a class with
no slot owns none of it. Moving it deletes the one-frame-per-module refusal -
test/compiler/native-regalloc.f TWO-FUNS-CASE is the case that reddens.

THE COST COMPARISON IS NOT A COMPARISON ANY MORE, and that is a simplification
rather than a shortcut. A re-emission is one instruction where the value is read
and a reload is one instruction where the value is read PLUS a store PLUS a frame
the routine may not otherwise need at all, so re-emission is never dearer and
usually cheaper. The rule is therefore eligibility alone: the class holds one
value and that value's defining operation is the move-wide. A sixty-four-bit
constant is a movz/movk chain tied by its own schema, so it is a class of MORE
than one value and is refused before the opcode is read - CODEGEN-CORPUS4:
BIG-CONSTS is protected by that test and by the opcode test after it.

THE DEFECT THIS LEAF WOULD HAVE SHIPPED WITHOUT, found with a tool and fixed:
src/compiler/native/migrate.f read the walk's SLOT count to decide whether the
lowering pass runs. Every plan row used to imply a slot, so one number stood for
two questions; a re-emission needs no slot, so a pure-remat walk looked like a
walk that had decided nothing, the lowering never ran, and the emitter was handed
the module the walk REFUSED to allocate (E-A64RAV-REGISTER, -8335). The frame
question stays A64RA:SPILLS; the decision question is A64RA:PLAN-N.
tools/codegen-alloc-dump.f is the tool that reads the two apart on a refused
migration and is where the next one should start.

WALLS, MEASURED THROUGH NMIGRATE. One-movz constants inside a loop body: master
compiles 13 and 14 and refuses 15 upward with -8508; this tree compiles every
count to 43 and stops at 44 with -6644, the migration context rather than a
register (dot habu-fit-a-rewritten-59aa92b7). The same constants ACROSS the loop
are refused at 14 exactly as before. tools/codegen-spill-probe.f straddles all
three walls; the scratch one is held out against habu-who-owns-the-82b7ceb2.

THE VALIDATOR LINK IS NOT BUILT AND IS NOT THIS LEAF'S. Deferral (2) above named
three candidates and the orchestrator leaned to handing VERIFY the pre-spill
module; that placement is WITHDRAWN on the tree's evidence. The check is a
two-module lockstep walk, it is already owned by habu-prove-the-spill-0294e0e8
with A64SPILL as its owner, and src/compiler/native/frozen.f holds ONE module
cursor for the whole chain by declared design. Re-emission does not open a new
gap - a wrong reload and a wrong immediate are the same unprovable statement from
one module - it becomes 0294e0e8's second consumer. What holds the immediate
today is execution: the chain's answers compared against the engine's own
compilation of the same text, and hand-derived arithmetic in
test/compiler/native-migrate.f REMAT-CASE.

Claim: agent=spillclose2 workspace=.jj-ws/habu-spill-close
