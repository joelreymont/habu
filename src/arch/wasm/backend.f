\ backend.f - WBACK, the Wasm provider for src/compiler/native/backend.f's
\ registry: the contracts it serves, its BINDING, and INSTALL with the rows of
\ src/arch/wasm/passes.f. It loads at run time: NCOMP's LOAD-PASSES requires
\ only the engine's own machine, so a driver or test compiling for Wasm requires
\ this file and calls INSTALL.

require src/lib/prelude.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/backend.f
require src/arch/wasm/passes.f

package WBACK
public

: ID ( -- CTARGET:backend-id ) 30 CTARGET:ID ;

\ memory32 under habu-wasm-cell64-v1, little-endian. A ptr64 contract is a
\ coherent Wasm target this backend was not written for: refused, not assumed.
\ Callers validate the contract first.
: SERVES? ( CTARGET:contract -- bool )
   CTARGET-CONTRACT:UNMAKE drop
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian p:CTARGET:ptr-width :}
   a CTARGET-ARCH:WASM CTARGET-ARCH:EQ
   b CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ABI:EQ and
   e CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ and
   p CTARGET-PTR--WIDTH:BITS32 CTARGET-PTR--WIDTH:EQ and ;

\ Overflow wraps, as i64 add, sub and mul do, and the selector refuses a
\ trapping unit; reals are bit-exact with contraction forbidden (docs/xcomp.md
\ "Floating point").
: BINDING ( -- CBIND:binding )
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32
   CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: INSTALL ( -- )
   ID CTARGET-ARCH:WASM [: SERVES? ;] [: SERVES? ;] CTARGET-BACKEND:MAKE
   WPASS:PASS NBACK:REGISTER ;

;package
