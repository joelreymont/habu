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
require src/compiler/native-effect.f
require src/compiler/native/machine.f
require src/compiler/native/a64ir.f
require src/compiler/native/frame.f

package NABI

private

\ The ABI field records the HOST PLATFORM IDENTITY (docs/porting.md), so each
\ supported target answers with its own convention. That a host has no native
\ backend loaded is a different question, asked later when a compilation
\ resolves the contract, and it is not this word's to answer.
: TARGET-ABI ( -- CTARGET:abi )
   HB-TARGET-LINUX? if CTARGET-ABI:AAPCS64-LINUX exit then
   HB-TARGET-MACOS? if CTARGET-ABI:AAPCS64-DARWIN exit then
   HB-TARGET-LINUX-X86-64? if CTARGET-ABI:SYSV-AMD64 exit then
   E-CTGT-ABI throw ;

public

\ The host AArch64 binding. Overflow wraps, as ARM64's add, sub and mul do; a
\ trapping unit is refused by the selector.
\ The architecture is this module's own: every routine it builds below names
\ NEFF register sets. On a host whose ABI is not an AArch64 one - linux-x86-64
\ answers sysv-amd64 above - CTARGET:CONTRACT refuses the pair with E-CTGT-ABI,
\ which is the true statement: that ABI is not one this architecture runs. The
\ x86-64 binding belongs to the x86-64 backend module, not to this file.
: BINDING ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 TARGET-ABI CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ `n` general registers starting at `base`. A set is a machine-free value, so a
\ run that crosses the data-stack, link, platform or zero register builds here
\ and is refused by the contract that would hold state in it - NEFF:ROUTINE is
\ where the machine is named. A caller states a run only to hold a routine UNDER
\ the machine's pool - which is what a pressure fixture is for. Production asks
\ SCRATCH.
: POOL ( n n -- NEFF:gprs )
   {: base:n n:n :}
   NEFF:GPR-NONE
   n 0 ?do base i + NEFF:GPR-REG NEFF:GPR-WITH loop ;

\ Every general register a routine of this convention may hold state in, which is
\ a fact about the machine and the engine and never a number a caller picks. The
\ set is not a run: src/habu/layout.f claims x19, x20, x26, x27 and x28 out of
\ the middle of it, so the largest run from x0 ends at x18 on Linux and x17 on
\ Darwin. The complete set is twenty-five registers on Linux and twenty-four on
\ Darwin, including x21..x25 and x29 on both. A routine that still does not fit
\ spills, which is the spill path's job and not the caller's to pre-empt.
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
\ The callee's own return goes to OUR caller, so the data stack needs no
\ instruction at the boundary. A tail branch is a B and not a Bl: no trait.
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

\ The same, for a word that also makes a call it comes back from.
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
