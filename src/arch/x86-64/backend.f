\ backend.f - pure x86-64 capability predicate.
\
\ Low-level dialect and emitter gates can use this without registering a
\ partially loaded provider. passes.f publishes the complete provider after
\ all of its callbacks exist.

require lib/prelude.f
require src/compiler/target.f

package X64BACK
public

: ID ( -- CTARGET:backend-id ) 20 CTARGET:ID ;

\ THE MACHINES THIS BACKEND SERVES. x86-64, little-endian, 64-bit addresses -
\ the layout src/compiler/native/x64ir.f builds and the x86-64 emitter writes.
\ The ABI is deliberately not part of it, for the reason the arm64 row gives:
\ which convention the host runs is src/compiler/native/abi.f's answer, not this
\ one. Byte order and pointer width are asked even though src/compiler/target.f
\ admits no other x86-64 contract today, because this predicate answers for the
\ machine this backend was written for and not for the table's current shape: a
\ contract the table widens to and this backend has not been taught is a
\ refusal, not an assumption.
\
\ Callers validate the contract before asking whether this implementation serves it.
: SERVES? ( CTARGET:contract -- bool )
   CTARGET-CONTRACT:UNMAKE drop
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian p:CTARGET:ptr-width :}
   a CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ
   e CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ and
   p CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ and ;

;package
