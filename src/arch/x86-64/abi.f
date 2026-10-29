\ abi.f - X64ABI, the x86-64 convention a Habu word is entered and left through,
\ as a routine contract; the x86-64 twin of src/compiler/native/abi.f. Argument i
\ comes from the caller's data-stack slot i and result j leaves in slot j. There
\ is no link register (`call` pushes the return address), so every contract says
\ NEFF-LINK:ABSENT; X64M:MACHINE refuses the ARM64 link answers.

require src/lib/prelude.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f
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

\ The allocator's spills, rounded to the stack alignment, and nothing else: the
\ return address `call` pushed is the saving, so a routine that spills nothing
\ has no frame however many calls it makes.
: FRAME-FOR ( n -- n )
   {: spills:n :}
   spills X64IR:SLOT-WIDTH *  X64M:MACHINE NMACH:FRAME-ROUND ;

public

\ Overflow wraps, as add, sub and imul do; the divide screens MIN-N -1 to wrap
\ too (src/compiler/native/emit-x64.f PUT-IDIV); the selector refuses a trapping
\ unit. NABI:BINDING answers this binding on an x86-64 host; this word answers it
\ on every host, so an ARM64 engine can compile x86-64 code.
: BINDING ( -- CBIND:binding )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ Nine of the sixteen general registers: rbx, rsp, rbp and r12..r15 are the
\ running engine's (src/compiler/native/x64ir.f REGFILE).
: SCRATCH ( -- NEFF:gprs )
   X64M:MACHINE NEFF:GPR-ALL ;

\ No register is part of the interface, so the whole pool is declared destroyed.
\ The whole float file is scratch: every engine float primitive moves its
\ arguments into the machine's float registers and its answer back out.
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
\ moves from; it makes no frame, as nothing is saved at the boundary.
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
\ The callee's own return goes to this word's caller, so the data stack needs no
\ instruction at the boundary. A tail branch is a jmp, not a call: no trait.
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
