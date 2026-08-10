\ frame.f - who owns which slot of a routine's own frame.
\
\ A routine of this chain has at most one frame, and until now two passes placed
\ things in it without either of them saying so. src/compiler/native/select.f
\ writes the caller's return address into slot zero of a calling routine's frame,
\ and src/compiler/native/regalloc.f hands out spill slots from offset zero
\ upward inside the frame the contract declares. Neither read the other, so a
\ routine that both called and spilled would have put a value on top of the
\ return address; it could not happen only because such a routine was refused
\ somewhere else (dot habu-give-the-routine-679de563).
\
\ THE LAYOUT IS ONE STATEMENT AND EVERY PASS READS IT HERE. The frame's SIZE is
\ the routine contract's declaration - src/compiler/a64-effect.f owns that, and
\ it is what every frame access is measured against. What is inside it is this
\ file's one rule, and it is derived from the contract too, so no pass has a
\ layout of its own to remember:
\
\   slot 0    the caller's return address, when the contract declares the direct
\             call trait AND declares that address still intact where control
\             leaves, and nothing at all when it does not;
\   above it  every slot the register allocator hands out, in the order it hands
\             them out.
\
\ WHY THE CONTRACT DECIDES IT RATHER THAN A FLAG SOMEWHERE. Whether a routine
\ keeps a return address in its frame is a question the contract already answers,
\ and it takes TWO of its fields to answer it. The trait says the routine
\ contains a direct call, which is what DESTROYS the caller's return address; the
\ link field says whether that address is still in x30 where control leaves,
\ which is what makes keeping a copy worth anything. A routine that calls and
\ preserves the link has to save and restore, so it owns the slot. A routine that
\ calls and declares the address destroyed never reads it back, so it owns
\ nothing - and src/compiler/a64-effect.f LINK-CK refuses that combination for
\ every routine control comes back from, so the only contract that reaches it is
\ one that never returns. Deriving the base here means the allocator's first slot
\ and the selector's link slot cannot disagree without the contract disagreeing
\ with itself.
\
\ WHAT IS NOT HERE. How big the frame has to BE is not derived from the program
\ yet: a routine whose author declared too small a frame is refused rather than
\ given the frame it needs (dot habu-derive-a-routine-84ed36b6). This file says
\ where things go inside whatever was declared, which is the half that decides
\ whether two owners collide.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f
require src/compiler/native/a64ir.f

package A64FRAME

private

\ The caller's return address is the first thing in the frame, so the offset a
\ calling routine's prologue saves it at is zero and the allocator's slots start
\ one slot above it.
0 constant LINK-IX

public

\ Does this routine's prologue keep the caller's return address in its own frame?
\ It is one question with one answer, and every pass that builds, places or
\ measures the save asks it here: the selector to decide whether to build the
\ pair at all, this file to decide where the allocator's slots start, and the
\ allocation validator to decide which frame accesses are the prologue's.
: LINK-KEPT? ( A64EFF:traits A64EFF:link -- bool )
   {: t:A64EFF:traits l:A64EFF:link :}
   t A64EFF:T-CALL A64EFF:TRAITS-HAS? 0= if false exit then
   l A64EFF-LINK:PRESERVED A64EFF-LINK:EQ ;

private

\ How many slots at the bottom of the frame the prologue owns: one for a routine
\ that keeps the caller's return address, none for one that does not.
: PROLOGUE-SLOTS ( A64EFF:traits A64EFF:link -- n )
   LINK-KEPT? if 1 else 0 then ;

public

\ The byte offset of the slot the caller's return address is kept in. It is a
\ slot of a calling routine's frame and of no other, so a reader that has not
\ decided the routine keeps one has no business asking for it - which is what
\ LINK-KEPT? above and SPILL-BASE below are for.
: LINK-SLOT ( -- n )
   LINK-IX A64IR:SLOT-WIDTH * ;

\ The byte offset of the first slot the register allocator may place a value in.
\ Everything below it belongs to the prologue, so a frame access naming less than
\ this is the prologue's own and one naming this or more is a spill. That is the
\ whole partition, and both sides read it here.
: SPILL-BASE ( A64EFF:traits A64EFF:link -- n )
   PROLOGUE-SLOTS A64IR:SLOT-WIDTH * ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
