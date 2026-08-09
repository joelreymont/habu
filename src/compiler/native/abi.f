\ abi.f - the convention a Habu word is entered and left through, as a routine
\ contract the chain's stages can be told. One concern: turning the four numbers
\ that describe a word - where its scratch registers start, how many it has, how
\ many values it takes and how many it leaves - into the contract the selector,
\ the allocator and the validator all answer about.
\
\ WHY IT IS PRODUCTION AND NOT A FIXTURE. Design section 7.6 says an externally
\ callable Habu word takes argument i out of data-stack slot i of the caller's
\ stack and leaves result j in slot j. That is a fact about the running engine,
\ not about a test: a routine compiled under any other convention cannot be
\ entered by the branch the interpreter uses, so publishing one as a word would
\ be publishing something the engine's callers cannot call. It lived in
\ test/compiler/native-chain-fixture.f while the only callers were suites; the
\ publication seam needs the same statement, and one statement is what makes the
\ suites and the seam answer about one convention.
\
\ THE POOL NEVER HOLDS AN ENGINE REGISTER. src/compiler/a64-effect.f keeps the
\ engine's data-stack pointer, the link register, the platform register and the
\ zero register out of every general-register set, so a contract that handed one
\ of them to the allocator cannot be built at all - the refusal is A64EFF's and
\ this file adds no check of its own.
\
\ AND THE FLOATING FILE IS ENTIRELY SCRATCH, WHICH IS A FACT ABOUT THE ENGINE
\ RATHER THAN A CONVENIENCE. Every float primitive the engine publishes is
\ self-contained: it pops its arguments off the data stack as cells, moves them
\ into d0 and d1, computes, moves the answer back into a general register and
\ pushes it (src/habu/habu1.f, BF+ through BF>S, and the decimal printer, which
\ uses d0 to d3 the same way). So nothing of the engine's lives in a floating
\ register across the branch that enters a word, and a word that destroys all
\ thirty-two destroys nothing anybody was keeping. That is why the pool is
\ declared here as the whole file rather than as a budget the caller states: the
\ general pool is a budget because the engine really does keep state in general
\ registers, and the floating pool is not because it does not. A64EFF's own
\ FPR-MASK already says no member of that file is reserved.
\
\ TWO CONTRACTS, AND THE DIFFERENCE IS ONE CAPABILITY. A leaf declares that it
\ preserves the link register and gets that for free. A routine that CALLS has to
\ make it true, so it declares the direct-call trait and a frame of one slot for
\ the caller's return address; the selector builds the save and the restore from
\ that declaration and the validator measures them against it.

require lib/prelude.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f
require src/compiler/a64-effect.f
require src/compiler/native/a64ir.f
require src/compiler/native/frame.f

package NABI

public

\ The AArch64 Darwin binding the native chain compiles under. Overflow wraps,
\ which is what ARM64's add, sub and mul do; a trapping unit is refused by the
\ selector.
: BINDING ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ `n` general registers starting at `base`.
: POOL ( n n -- A64EFF:gprs )
   {: base:n n:n :}
   A64EFF:GPR-NONE
   n 0 ?do base i + A64EFF:GPR-REG A64EFF:GPR-WITH loop ;


\ The first `n` data-stack slots of the caller's stack, as one side of a calling
\ convention. Both sides of a Habu word's convention have this shape, so one word
\ builds either list.
\
\ AND THE LIST IS NOT WHAT SAYS SO. Every constructor below DECLARES
\ A64EFF-CONV:DSTACK, because at `n` of zero this list is empty and an empty list
\ is silent: it is what a routine passing nothing has under either convention.
\ This file is the only production writer of a routine contract, so this is where
\ the fact that every Habu word is entered through the caller's data stack is
\ written down, once, for the selector, the allocator and the validator to read.
: SLOT-SEQ ( n -- A64EFF:placeseq )
   {: n:n :}
   A64EFF:SEQ-NONE
   n 0 ?do i A64EFF:SEQ-WITH-SLOT loop ;

\ How deep a frame a routine of this convention declares: what its prologue owns
\ plus the slots the register allocator may need, rounded up to the stack
\ alignment. src/compiler/native/frame.f says which slots are whose, so this is
\ the one place that turns a count of spill slots into a size and both halves of
\ the layout come off the same declaration. A routine whose prologue owns nothing
\ and that needs no slot declares no frame at all.
: FRAME-FOR ( A64EFF:traits n -- n )
   {: t:A64EFF:traits spills:n :}
   t A64FRAME:SPILL-BASE  spills A64IR:SLOT-WIDTH *  +  A64EFF:FRAME-ROUND ;

\ A leaf word under that convention, with room in its frame for `spills` values
\ the register allocator could not keep in registers. No register is part of the
\ interface - everything arrives and leaves through the caller's stack - so the
\ pool is exactly the `n` scratch registers from `base` and the whole of it is
\ declared destroyed.
: LEAF-FRAMED ( n n n n n -- A64EFF:routine )
   {: base:n n:n in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   base n POOL
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE
   A64EFF:TRAITS-NONE spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: LEAF ( n n n n -- A64EFF:routine )
   0 LEAF-FRAMED ;

\ The same convention for a word that calls, with the same room for spills above
\ the link slot. Two fields change against the leaf and neither is decoration:
\ the direct-call trait is what the selector builds the frame and the link save
\ from, and it is also what A64FRAME reads to put the allocator's slots above the
\ return address. `link preserved` does not change, and that is the point - a
\ leaf has it for nothing and a caller has to make it true.
: CALL-FRAMED ( n n n n n -- A64EFF:routine )
   {: base:n n:n in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   base n POOL
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:T-CALL
   A64EFF:T-CALL spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: CALL ( n n n n -- A64EFF:routine )
   0 CALL-FRAMED ;

\ ---- and the same convention for a word that LEAVES through its last callee ---
\ A word whose last operation is a call whose results are already its own results
\ does not have to come back for them. It can branch, and let the callee's own
\ return go to OUR caller: the arguments the callee reads are the cells this
\ word's caller will read the results out of, so the data stack needs no
\ instruction at the boundary at all.
\
\ ONE FIELD CHANGES AGAINST THE TWO ABOVE, AND IT IS THE ONE THAT SAYS SO.
\ `control tail-call` is what src/compiler/a64-effect.f already models - a routine
\ control does not come back to, that still delivers a result and still needs the
\ caller's return address intact, which is exactly what RETURNING? answers true
\ for and LINK-CK demands. Everything else is the convention it always was.
\
\ THE TRAITS ARE THE OTHER HALF, AND THEY ARE NOT THE SAME QUESTION. A trait
\ names an instruction FORM the routine contains, and a tail branch is a B and
\ not a Bl - so a word whose only call is the tail one contains no direct call,
\ reserves no frame and saves no return address, which is the whole of the win.
\ A word that calls somebody else BEFORE it tail-branches does contain one, and
\ declares it, and pays for it exactly as CALL-FRAMED's routines do; its epilogue
\ then stands in front of the branch instead of in front of a return.
: TAIL-FRAMED ( n n n n n -- A64EFF:routine )
   {: base:n n:n in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   base n POOL
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:TAIL-CALL
   A64EFF:TRAITS-NONE
   A64EFF:TRAITS-NONE spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: TAIL ( n n n n -- A64EFF:routine )
   0 TAIL-FRAMED ;

\ The same, for a word that also makes a call it comes back from.
: TAIL-CALLING-FRAMED ( n n n n n -- A64EFF:routine )
   {: base:n n:n in:n out:n spills:n :}
   A64EFF-CONV:DSTACK
   in SLOT-SEQ  out SLOT-SEQ
   base n POOL
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:CLOBBERED A64EFF-LINK:PRESERVED A64EFF-CONTROL:TAIL-CALL
   A64EFF:T-CALL
   A64EFF:T-CALL spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: TAIL-CALLING ( n n n n -- A64EFF:routine )
   0 TAIL-CALLING-FRAMED ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
