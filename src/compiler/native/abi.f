\ abi.f - the convention a Habu word is entered and left through, as a routine
\ contract the chain's stages can be told. One concern: turning the four numbers
\ that describe a word - where its scratch registers start, how many it has, how
\ many values it takes and how many it leaves - into the contract the selector,
\ the allocator and the validator all answer about.
\
\ Design 7.6: an externally callable Habu word takes argument i out of data-stack
\ slot i of the caller's stack and leaves result j in slot j.
\
\ The whole floating file is scratch: every engine float primitive moves its
\ arguments into d0/d1 and its answer back out, so nothing lives across a call.

require lib/prelude.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f
require src/compiler/a64-effect.f
require src/compiler/native/a64ir.f
require src/compiler/native/frame.f

package NABI

public

\ The AArch64 Darwin binding. Overflow wraps, as ARM64's add, sub and mul do; a
\ trapping unit is refused by the selector.
: BINDING ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ `n` general registers starting at `base`. A64EFF keeps the data-stack, link,
\ platform and zero registers out of every set, so none of them can arrive here.
\ A caller states a run only to hold a routine UNDER the machine's pool - which
\ is what a pressure fixture is for. Production asks SCRATCH.
: POOL ( n n -- A64EFF:gprs )
   {: base:n n:n :}
   A64EFF:GPR-NONE
   n 0 ?do base i + A64EFF:GPR-REG A64EFF:GPR-WITH loop ;

\ Every general register a routine of this convention may hold state in, which is
\ a fact about the machine and the engine and never a number a caller picks. The
\ set is not a run: src/habu/layout.f claims x19, x20, x26, x27 and x28 out of
\ the middle of it, so the largest run from x0 stops at x18 and leaves six
\ registers - x21..x25 and x29 - that no caller could ever name. Measured on a
\ twenty-four-value body: six values went to frame slots while those six sat
\ idle. A routine that still does not fit spills, which is the spill path's job
\ and not the caller's to pre-empt.
: SCRATCH ( -- A64EFF:gprs )
   A64EFF:GPR-ALL ;

\ At n of zero the list is empty and an empty list is silent, so every constructor
\ below declares A64EFF-CONV:DSTACK rather than leaving the list to say it.
: SLOT-SEQ ( n -- A64EFF:placeseq )
   {: n:n :}
   A64EFF:SEQ-NONE
   n 0 ?do i A64EFF:SEQ-WITH-SLOT loop ;

\ Prologue slots plus the allocator's, rounded to the stack alignment. It takes
\ the link declaration as well as the trait, because A64FRAME reads both.
: FRAME-FOR ( A64EFF:traits A64EFF:link n -- n )
   {: t:A64EFF:traits l:A64EFF:link spills:n :}
   t l A64FRAME:SPILL-BASE  spills A64IR:SLOT-WIDTH *  +  A64EFF:FRAME-ROUND ;

\ No register is part of the interface, so the whole pool is declared destroyed.
: LEAF-FRAMED ( A64EFF:gprs n n n -- A64EFF:routine )
   {: pool:A64EFF:gprs in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE
   A64EFF:TRAITS-NONE A64EFF-LINK:PRESERVED spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: LEAF ( A64EFF:gprs n n -- A64EFF:routine )
   0 LEAF-FRAMED ;

\ The direct-call trait is what the selector builds the frame and the link save
\ from. `link preserved` does not change: a caller has to make it true.
: CALL-FRAMED ( A64EFF:gprs n n n -- A64EFF:routine )
   {: pool:A64EFF:gprs in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:T-CALL
   A64EFF:T-CALL A64EFF-LINK:PRESERVED spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: CALL ( A64EFF:gprs n n -- A64EFF:routine )
   0 CALL-FRAMED ;

\ ---- and the same convention for a word that LEAVES through its last callee ---
\ The callee's own return goes to OUR caller, so the data stack needs no
\ instruction at the boundary. A tail branch is a B and not a Bl: no trait.
: TAIL-FRAMED ( A64EFF:gprs n n n -- A64EFF:routine )
   {: pool:A64EFF:gprs in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:TAIL-CALL
   A64EFF:TRAITS-NONE
   A64EFF:TRAITS-NONE A64EFF-LINK:PRESERVED spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: TAIL ( A64EFF:gprs n n -- A64EFF:routine )
   0 TAIL-FRAMED ;

\ The same, for a word that also makes a call it comes back from.
: TAIL-CALLING-FRAMED ( A64EFF:gprs n n n -- A64EFF:routine )
   {: pool:A64EFF:gprs in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:TAIL-CALL
   A64EFF:T-CALL
   A64EFF:T-CALL A64EFF-LINK:PRESERVED spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: TAIL-CALLING ( A64EFF:gprs n n -- A64EFF:routine )
   0 TAIL-CALLING-FRAMED ;

\ ---- and the convention of a word control never comes back from ---------------
\ No Ret anywhere: nothing reads the return address back and no unwinder walks
\ the frame, so the link is declared destroyed and the delta is the whole frame.
: NORET-FRAMED ( A64EFF:gprs n n n -- A64EFF:routine )
   {: pool:A64EFF:gprs in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:CLOBBERED A64EFF-CONTROL:NO-RETURN
   A64EFF:T-CALL
   A64EFF:T-CALL A64EFF-LINK:CLOBBERED spills FRAME-FOR
   dup negate A64EFF:ROUTINE ;

\ A `begin … again` loop: no exit edge and no call at all, so no trait - which is
\ the field the selector holds against the module. `control no-return` still holds.
: NORET-LEAF-FRAMED ( A64EFF:gprs n n n -- A64EFF:routine )
   {: pool:A64EFF:gprs in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:NO-RETURN
   A64EFF:TRAITS-NONE
   A64EFF:TRAITS-NONE A64EFF-LINK:PRESERVED spills FRAME-FOR
   dup negate A64EFF:ROUTINE ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
