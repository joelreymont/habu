\ backend.f - pure arm64 capability predicate.
\
\ Low-level dialect and emitter gates can use this without registering a
\ partially loaded provider. passes.f publishes the complete provider after
\ all of its callbacks exist.

require lib/prelude.f
require src/compiler/target.f

package A64BACK
public

: ID ( -- CTARGET:backend-id ) 10 CTARGET:ID ;

\ THE MACHINES THIS BACKEND SERVES. AArch64, little-endian, 64-bit addresses -
\ the layout src/compiler/native/a64ir.f builds and src/compiler/native/emit.f
\ writes. The ABI is deliberately not part of it: both AAPCS64 variants are
\ served, and which one the host runs is src/compiler/native/abi.f's answer, not
\ this one. A big-endian AArch64 contract is a coherent machine this backend
\ does not serve, which is a different refusal from an architecture that has no
\ backend loaded at all.
\
\ Callers validate the contract before asking whether this implementation serves it.
: SERVES? ( CTARGET:contract -- bool )
   CTARGET-CONTRACT:UNMAKE drop
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian p:CTARGET:ptr-width :}
   a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   e CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ and
   p CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ and ;

;package
