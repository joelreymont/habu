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
   t A64FRAME:SPILL-BASE  spills A64IR:SLOT-WIDTH *  + {: want:n :}
   want 0= if 0 exit then
   want A64EFF:SP-ALIGN 1- +  A64EFF:SP-ALIGN /  A64EFF:SP-ALIGN * ;

\ A leaf word under that convention, with room in its frame for `spills` values
\ the register allocator could not keep in registers. No register is part of the
\ interface - everything arrives and leaves through the caller's stack - so the
\ pool is exactly the `n` scratch registers from `base` and the whole of it is
\ declared destroyed.
: LEAF-FRAMED ( n n n n n -- A64EFF:routine )
   {: base:n n:n in:n out:n spills:n :}
   in SLOT-SEQ  out SLOT-SEQ
   base n POOL
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
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
   in SLOT-SEQ  out SLOT-SEQ
   base n POOL
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:T-CALL
   A64EFF:T-CALL spills FRAME-FOR
   0 A64EFF:ROUTINE ;

: CALL ( n n n n -- A64EFF:routine )
   0 CALL-FRAMED ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
