\ cp-async-legal.f - checked cp.async pipeline target-legality gate (package CPLEGAL).
\
\ The cp.async double/single-buffered software pipeline (lib/ptx/cg-mma.f,
\ cg-matmul-emit.f) is only lowerable onto a target that actually has the async
\ copy engine, the block barrier its WAIT drains behind, and enough per-block
\ shared memory to hold every staged buffer. Those facts live in the target
\ descriptor (maki/target/target.f: CAP-ASYNC, CAP-BARRIER, SHARED@), but nothing
\ consulted them at the pipeline decision - the emitters hardcode PTX-HEADER
\ and MMA-CHECK-SMEM compares against a hardcoded 48 KiB constant, not the target's
\ real budget. This gate closes that gap: given a pipeline's per-stage buffer bytes
\ (bufb, from the tile geometry - lib/ptx MMA-BUFB) and its depth (stages), it
\ REQUIREs the selected target descriptor and rejects fail-closed when the target
\ cannot support the pipeline.
\
\ SCOPE - this is a static depth-vs-target LEGALITY check, NOT an emit-time
\ per-slot typestate. The double-buffered emit-time cpp-slot typestate was
\ permanently refuted (dot habu-wire-cppslot-typestate-ce2463df: the software
\ pipeline's commit and wait land on DIFFERENT slots, so no single emit-time token
\ can carry commit->wait->read across depth>1). Pipeline-DEPTH legality is the
\ honest remaining shape: `bufb * stages` shared bytes and the async+barrier caps
\ must fit the target, whatever the runtime slot alternation does. It composes with
\ - it does not duplicate - the per-slot ordering typestate (CPPSLOT, lib/ptx/
\ cpp-slot.f) and the block-uniform barrier model (M5/M5b).
\
\ The core predicate works on a raw descriptor value (LEGAL-DESC? / REQUIRE-DESC)
\ so a legality probe never registers into the append-only, capped target registry;
\ REQUIRE is the production entry over an interned target id.

require maki/target/target.f

-5082 constant E-CP-ASYNC-TGT   \ cp.async pipeline illegal on the selected target

package CPLEGAL
public

\ total staged shared bytes a depth-`stages` pipeline needs: one cp.async buffer
\ (bufb) replicated once per pipeline stage.
: STAGED-BYTES ( n n -- n )   * ;   \ ( bufb stages -- bytes )

\ the caps a cp.async software pipeline demands: the async copy engine AND the
\ block barrier the WAIT drains behind (M5 committed->ready fence).
: NEEDED-CAPS ( -- n )   TARGET:CAP-ASYNC TARGET:CAP-BARRIER or ;

\ every needed cap bit is present in the target's advertised capability bitset.
: CAPS-OK? ( n -- bool )   NEEDED-CAPS tuck and = ;   \ ( caps -- bool )

\ the staged buffers fit the target's per-block shared-memory budget.
: FITS? ( n n TARGET:descriptor -- bool )   \ ( bufb stages descriptor -- bool )
   TARGET:DESC-SHARED@ >r STAGED-BYTES r> <= ;

\ the target advertises the async + barrier capabilities the pipeline needs.
: CAPS-PRESENT? ( TARGET:descriptor -- bool )
   TARGET:DESC-CAPS@ CAPS-OK? ;

\ legality over a raw (un-interned) descriptor value: caps present AND buffers fit.
: LEGAL-DESC? ( n n TARGET:descriptor -- bool )   \ ( bufb stages descriptor -- bool )
   dup >r FITS? r> CAPS-PRESENT? and ;

\ fail-closed gate over a raw descriptor value.
: REQUIRE-DESC ( n n TARGET:descriptor -- )   \ ( bufb stages descriptor -- )
   LEGAL-DESC? 0= if E-CP-ASYNC-TGT throw then ;

\ production entry: gate a pipeline of `stages` x `bufb` bytes against an interned
\ target id, before lowering. Throws E-CP-ASYNC-TGT on an unsupported target.
: REQUIRE ( n n CAD-KIND:target-id -- )   \ ( bufb stages target-id -- )
   TARGET:DESCRIPTOR@ REQUIRE-DESC ;

;package
