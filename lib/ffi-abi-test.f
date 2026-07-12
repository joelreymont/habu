\ ffi-abi-test.f - target-independent FFI ABI and marshalling tests.
\ Run: bin/hb --load lib/ffi-abi-test.f

require lib/test.f
require lib/ffi.f

create FFI-T-OUT 1 cells allot
create FFI-T-KP-CELL 1 cells allot

TRUSTED: FFI-T-STORE-X1 ( -- n ) cp@ {: fn:n :}
   $F9000001 fn patch32
   $D65F03C0 fn $4 + patch32
   fn ;

TRUSTED: FFI-T-STORE ( ptr a n -- n ) {: out:ptr value:n :}
   FFI:RESET  out 8 0 FFI:WRITABLE!  value 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 FFI-T-STORE-X1 ffi-call-bounded ;

TRUSTED: FFI-T-KPARAM-SUM2 ( -- n ) cp@ {: fn:n :}
   $F9400009 fn       patch32
   $F940040A fn $4 +  patch32
   $F9400129 fn $8 +  patch32
   $F940014A fn $C +  patch32
   $8B0A0120 fn $10 + patch32
   $D65F03C0 fn $14 + patch32
   fn ;

TRUSTED: FFI-T-X8-STORE ( -- n ) cp@ {: fn:n :}
   $F9000100 fn patch32
   $D65F03C0 fn $4 + patch32
   fn ;

TRUSTED: FFI-T-STACK-STORE ( -- n ) cp@ {: fn:n :}
   $F94003E9 fn patch32
   $F9000120 fn $4 + patch32
   $D65F03C0 fn $8 + patch32
   fn ;

TRUSTED: FFI-T-X8-CALL ( ptr a -- n ) {: out:ptr :}
   FFI:RESET
   42 0 FFI:VALUE!
   out 8 FFI:X8-WRITABLE!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 FFI-T-X8-STORE ffi-call-abi-bounded ;

TRUSTED: FFI-T-STACK-CALL ( ptr a -- n ) {: out:ptr :}
   FFI:RESET
   77 0 FFI:VALUE!
   out 8 0 FFI:STACK-WRITABLE!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   1 FFI-T-STACK-STORE ffi-call-abi-bounded ;

TRUSTED: FFI-T-KPARAM-CALL ( ptr a -- n ) {: params:ptr :}
   FFI:RESET
   params 0 FFI:READABLE!
   FFI:ARGS FFI:REG-LENS 1 FFI-T-KPARAM-SUM2 ffi-call-bounded ;

: FFI-T-OUT-PARAM ( -- )
   0 FFI-T-OUT FFI:OUT!
   FFI-T-OUT 99 FFI-T-STORE drop
   FFI-T-OUT FFI:OUT@ 99 T=
   [: FFI-T-OUT 0 0 FFI:WRITABLE! ;] E-FFI-ARITY TTHROWSQ ;

: FFI-T-KPARAM-CAP ( -- )
   FFI:KPARAM-RESET
   [: 17 0 do i FFI:KPARAM-VALUE+ loop ;] E-FFI-ARITY TTHROWSQ ;

\ Regression: the evaluate throw-unwind cell (EVALREC-CELL, src/habu/layout.f) must
\ live outside the FFI task-DATA block [FFI:BUF-OFF, FFI:KPARAM-END-OFF). An
\ FFI call fills that block, so an overlap silently clobbers the branch target and a
\ throw crossing an evaluate boundary (any FFI-using program run under include) jumps
\ to a data address. Assert the two regions are disjoint at build time.
: FFI-T-EVALREC-DISJOINT ( -- )
   EVALREC-CELL FFI:BUF-OFF <
   EVALREC-CELL FFI:KPARAM-END-OFF >= or TTRUE ;

: FFI-T-KPARAMS ( -- )
   FFI:KPARAM-RESET
   13 FFI:KPARAM-VALUE+
   21 FFI-T-KP-CELL FFI:OUT!
   FFI-T-KP-CELL FFI:KPARAM+
   FFI:KPARAM-COUNT 2 T=
   FFI:KPARAMS drop FFI-T-KPARAM-CALL 34 T=
   FFI-T-KPARAM-CAP
   FFI:KPARAM-RESET
   FFI:KPARAM-COUNT 0 T= ;

: FFI-ABI-RUN ( -- )
   T-RESET
   FFI-T-EVALREC-DISJOINT
   FFI-T-OUT-PARAM
   0 FFI-T-OUT FFI:OUT!
   FFI-T-OUT FFI-T-X8-CALL drop
   FFI-T-OUT @ 42 T=
   0 FFI-T-OUT FFI:OUT!
   FFI-T-OUT FFI-T-STACK-CALL drop
   FFI-T-OUT @ 77 T=
   FFI-T-KPARAMS
   T-REPORT ;

FFI-ABI-RUN
