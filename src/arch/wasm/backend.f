\ backend.f - WBACK, the Wasm backend as a provider in the compiler's registry
\ (src/compiler/native/backend.f): the contracts it serves, the binding a
\ definition compiles under for it, and the install that publishes it with its
\ rows (src/arch/wasm/passes.f). It is to src/arch/wasm what src/arch/x86-64's
\ backend.f, abi.f BINDING and passes.f INSTALL are to that machine.
\
\ IT LOADS AT RUN TIME. NCOMP's LOAD-PASSES requires the engine's own machine's
\ rows only (src/compiler/native/compiler.f); a driver or a test that compiles
\ for Wasm requires this file and calls INSTALL. The registry, the shadow and
\ the driver take this row as they take any other, and a definition reaches it
\ by opening a shadow on BINDING (src/compiler/native/shadow.f).

require lib/prelude.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/backend.f
require src/arch/wasm/passes.f

package WBACK
public

: ID ( -- CTARGET:backend-id ) 30 CTARGET:ID ;

\ THE CONTRACTS THIS BACKEND SERVES: Wasm under habu-wasm-cell64-v1,
\ little-endian, with 32-bit addresses, the memory32 layout WPROF pins and the
\ encoder writes. A ptr64 contract is a coherent Wasm target
\ (src/compiler/target.f) that this backend was not written for, so it is a
\ refusal, not an assumption. Callers validate the contract first.
: SERVES? ( CTARGET:contract -- bool )
   CTARGET-CONTRACT:UNMAKE drop
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian p:CTARGET:ptr-width :}
   a CTARGET-ARCH:WASM CTARGET-ARCH:EQ
   b CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ABI:EQ and
   e CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ and
   p CTARGET-PTR--WIDTH:BITS32 CTARGET-PTR--WIDTH:EQ and ;

\ The binding a Wasm definition compiles under. Overflow wraps, as i64 add,
\ sub and mul do, and the selector refuses a trapping unit; reals are IEEE 754,
\ bit-exact, with contraction forbidden (docs/wasm-backend.md 8.4).
: BINDING ( -- CBIND:binding )
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32
   CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ Registers the provider with its rows.
: INSTALL ( -- )
   ID CTARGET-ARCH:WASM [: SERVES? ;] [: SERVES? ;] CTARGET-BACKEND:MAKE
   WPASS:PASS NBACK:REGISTER ;

;package
