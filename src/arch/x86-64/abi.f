\ abi.f - the convention a Habu word is entered and left through on x86-64, as a
\ routine contract the chain's stages can be told. It is to src/arch/x86-64 what
\ src/compiler/native/abi.f is to ARM64, and the same four numbers describe a
\ word: where its scratch registers come from, how many values it takes, how
\ many it leaves and how many it spills.
\
\ Design 7.6 is unchanged by the machine: an externally callable Habu word takes
\ argument i out of data-stack slot i of the caller's stack and leaves result j
\ in slot j. What the machine changes is three fields, and each of them is a
\ fact about x86-64 rather than a choice this file makes:
\
\ - THERE IS NO LINK REGISTER, so every contract here says NEFF-LINK:ABSENT.
\   `call` pushes the return address on the machine stack; there is no register
\   holding it for a routine to preserve or clobber, and X64M:MACHINE refuses
\   the two ARM64 answers exactly as A64M:MACHINE refuses this one.
\ - THE PROLOGUE SAVES NOTHING. On ARM64 a routine that calls spends the first
\   frame slot on x30; here the eight bytes `call` pushed ARE that saving, so a
\   frame holds the allocator's spills and nothing else, and a routine that
\   spills nothing declares no frame however many calls it makes.
\ - THE POOL IS NINE REGISTERS. Seven of the sixteen are the running engine's
\   (rbx, rsp, rbp, r12..r15), which src/compiler/native/x64ir.f REGFILE
\   decides and X64M:MACHINE NEFF:GPR-ALL reports.
\
\ The whole floating file is scratch here for the reason it is there: every
\ engine float primitive moves its arguments into the machine's own float
\ registers and its answer back out, so nothing lives across a call.

require lib/prelude.f
require src/compiler/native-effect.f
require src/compiler/native/machine.f
require src/compiler/native/x64ir.f
require src/arch/x86-64/machine.f

package X64ABI

private

\ At n of zero the list is empty and an empty list is silent, so every
\ constructor below declares NEFF-CONV:DSTACK rather than leaving the list to
\ say it.
: SLOT-SEQ ( n -- NEFF:placeseq )
   NEFF:SEQ-DSTACK ;

\ The allocator's spills, rounded to the stack alignment. There is no prologue
\ slot to add: the return address is on the machine stack and not in this frame.
: FRAME-FOR ( n -- n )
   {: spills:n :}
   spills X64IR:SLOT-WIDTH *  X64M:MACHINE NMACH:FRAME-ROUND ;

public

\ Every general register a routine of this convention may hold state in, which is
\ a fact about the machine and the engine and never a number a caller picks.
: SCRATCH ( -- NEFF:gprs )
   X64M:MACHINE NEFF:GPR-ALL ;

\ No register is part of the interface, so the whole pool is declared destroyed.
: LEAF-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE X64M:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE
   spills FRAME-FOR
   0 X64M:MACHINE NEFF:ROUTINE ;

: LEAF ( NEFF:gprs n n -- NEFF:routine )
   0 LEAF-FRAMED ;

\ The direct-call trait is what the selector builds a call site's data-stack
\ moves from. It does NOT make a frame here: nothing is saved at the boundary.
: CALL-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE X64M:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:RETURNS
   NEFF:T-CALL
   spills FRAME-FOR
   0 X64M:MACHINE NEFF:ROUTINE ;

: CALL ( NEFF:gprs n n -- NEFF:routine )
   0 CALL-FRAMED ;

\ ---- and the same convention for a word that LEAVES through its last callee ---
\ The callee's own return goes to OUR caller, so the data stack needs no
\ instruction at the boundary. A tail branch is a jmp and not a call: no trait.
: TAIL-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE X64M:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:TAIL-CALL
   NEFF:TRAITS-NONE
   spills FRAME-FOR
   0 X64M:MACHINE NEFF:ROUTINE ;

: TAIL ( NEFF:gprs n n -- NEFF:routine )
   0 TAIL-FRAMED ;

\ The same, for a word that also makes a call it comes back from.
: TAIL-CALLING-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE X64M:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:TAIL-CALL
   NEFF:T-CALL
   spills FRAME-FOR
   0 X64M:MACHINE NEFF:ROUTINE ;

\ ---- and the convention of a word control never comes back from ---------------
\ No return anywhere, so the whole frame is the delta a caller's unwinder would
\ have to account for.
: NORET-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE X64M:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:NO-RETURN
   NEFF:T-CALL
   spills FRAME-FOR
   dup negate X64M:MACHINE NEFF:ROUTINE ;

\ A `begin … again` loop: no exit edge and no call at all, so no trait - which is
\ the field the selector holds against the module. `control no-return` still holds.
: NORET-LEAF-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE X64M:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:NO-RETURN
   NEFF:TRAITS-NONE
   spills FRAME-FOR
   dup negate X64M:MACHINE NEFF:ROUTINE ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
