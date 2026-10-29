\ backend.f - pure x86-64 capability predicate, usable by dialect and emitter
\ gates before the provider is registered; passes.f publishes the provider once
\ all of its callbacks exist.

require src/lib/prelude.f
require src/compiler/target.f

package X64BACK
public

: ID ( -- CTARGET:backend-id ) 20 CTARGET:ID ;

\ x86-64, little-endian, 64-bit addresses; the ABI is not checked
\ (src/compiler/native/abi.f picks it). Byte order and width are checked though
\ src/compiler/target.f admits no other x86-64 contract: a contract the table
\ widens to is refused, not assumed. Callers validate the contract first.
: SERVES? ( CTARGET:contract -- bool )
   CTARGET-CONTRACT:UNMAKE drop
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian p:CTARGET:ptr-width :}
   a CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ
   e CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ and
   p CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ and ;

;package
