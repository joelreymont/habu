\ abi.f - the convention a Habu word is entered and left through, built as a
\ routine contract from four numbers: scratch base, scratch count, values taken
\ and values left. An externally callable word takes argument i from data-stack
\ slot i and leaves result j in slot j. The whole floating file is scratch:
\ every engine float primitive moves its operands through d0/d1.

require src/lib/prelude.f
require src/compiler/target/native-binding.f
require src/compiler/native-effect.f
require src/compiler/native/machine.f
require src/compiler/native/a64ir.f
require src/compiler/native/frame.f

package NABI
public

: BINDING ( -- CBIND:binding )
   RTARGET:HOST-TARGET RTARGET:NATIVE-BINDING ;

\ `n` general registers from `base`, machine-free: a run crossing a reserved
\ register is refused by the contract that holds state in it (NEFF:ROUTINE).
\ Only pressure fixtures state a run; production asks SCRATCH.
: POOL ( n n -- NEFF:gprs )
   {: base:n n:n :}
   NEFF:GPR-NONE
   n 0 ?do base i + NEFF:GPR-REG NEFF:GPR-WITH loop ;

\ The set is not a run: src/habu/layout.f claims x19, x20, x26, x27 and x28, so
\ it holds twenty-five registers on Linux and twenty-four on Darwin (x18).
: SCRATCH ( -- NEFF:gprs )
   A64IR:MACHINE NEFF:GPR-ALL ;

\ At n of zero the list is empty and an empty list is silent, so every constructor
\ below declares NEFF-CONV:DSTACK rather than leaving the list to say it.
: SLOT-SEQ ( n -- NEFF:placeseq )
   NEFF:SEQ-DSTACK ;

\ Prologue slots plus the allocator's, rounded to the stack alignment. It takes
\ the link declaration as well as the trait, because A64FRAME reads both.
: FRAME-FOR ( NEFF:traits NEFF:link n -- n )
   {: t:NEFF:traits l:NEFF:link spills:n :}
   t l A64FRAME:SPILL-BASE  spills A64IR:SLOT-WIDTH *  +  A64IR:MACHINE NMACH:FRAME-ROUND ;

\ No register is part of the interface, so the whole pool is declared destroyed.
: LEAF-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE A64IR:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE
   NEFF:TRAITS-NONE NEFF-LINK:PRESERVED spills FRAME-FOR
   0 A64IR:MACHINE NEFF:ROUTINE ;

: LEAF ( NEFF:gprs n n -- NEFF:routine )
   0 LEAF-FRAMED ;

\ The direct-call trait is what the selector builds the frame and the link save
\ from. `link preserved` does not change: a caller has to make it true.
: CALL-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE A64IR:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:T-CALL
   NEFF:T-CALL NEFF-LINK:PRESERVED spills FRAME-FOR
   0 A64IR:MACHINE NEFF:ROUTINE ;

: CALL ( NEFF:gprs n n -- NEFF:routine )
   0 CALL-FRAMED ;

\ ---- and the same convention for a word that LEAVES through its last callee ---
\ The callee's own return goes to OUR caller. Selection moves the data-stack
\ pointer to the callee's argument base before branching, when needed.
\ A tail branch is a B and not a Bl: no trait.
: TAIL-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE A64IR:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:PRESERVED NEFF-CONTROL:TAIL-CALL
   NEFF:TRAITS-NONE
   NEFF:TRAITS-NONE NEFF-LINK:PRESERVED spills FRAME-FOR
   0 A64IR:MACHINE NEFF:ROUTINE ;

: TAIL ( NEFF:gprs n n -- NEFF:routine )
   0 TAIL-FRAMED ;

: TAIL-CALLING-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE A64IR:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:PRESERVED NEFF-CONTROL:TAIL-CALL
   NEFF:T-CALL
   NEFF:T-CALL NEFF-LINK:PRESERVED spills FRAME-FOR
   0 A64IR:MACHINE NEFF:ROUTINE ;

: TAIL-CALLING ( NEFF:gprs n n -- NEFF:routine )
   0 TAIL-CALLING-FRAMED ;

\ ---- and the convention of a word control never comes back from ---------------
\ No Ret anywhere: nothing reads the return address back and no unwinder walks
\ the frame, so the link is declared destroyed and the delta is the whole frame.
: NORET-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE A64IR:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:CLOBBERED NEFF-CONTROL:NO-RETURN
   NEFF:T-CALL
   NEFF:T-CALL NEFF-LINK:CLOBBERED spills FRAME-FOR
   dup negate A64IR:MACHINE NEFF:ROUTINE ;

\ A `begin … again` loop: no exit edge and no call at all, so no trait - which is
\ the field the selector holds against the module. `control no-return` still holds.
: NORET-LEAF-FRAMED ( NEFF:gprs n n n -- NEFF:routine )
   {: pool:NEFF:gprs in:n out:n spills:n :}
   NEFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   NEFF:FPR-NONE NEFF:FPR-NONE A64IR:MACHINE NEFF:FPR-ALL
   NEFF-NZCV:CLOBBERED NEFF-LINK:PRESERVED NEFF-CONTROL:NO-RETURN
   NEFF:TRAITS-NONE
   NEFF:TRAITS-NONE NEFF-LINK:PRESERVED spills FRAME-FOR
   dup negate A64IR:MACHINE NEFF:ROUTINE ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
