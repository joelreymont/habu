\ backend.f - pure arm64 capability predicate, usable by dialect and emitter
\ gates before the provider is registered; passes.f publishes the provider once
\ all of its callbacks exist.

require src/lib/prelude.f
require src/compiler/target.f

package A64BACK
public

: ID ( -- CTARGET:backend-id ) 10 CTARGET:ID ;

\ AArch64, little-endian, 64-bit addresses. The ABI is not checked: both AAPCS64
\ variants are served and src/compiler/native/abi.f picks the host's. Callers
\ validate the contract first.
: SERVES? ( CTARGET:contract -- bool )
   CTARGET-CONTRACT:UNMAKE drop
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian p:CTARGET:ptr-width :}
   a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   e CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ and
   p CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ and ;

;package
