\ ffi-abi-test.f - target-independent FFI ABI and marshalling tests.
\ Run: bin/hb --load lib/ffi-abi-test.f

require lib/test.f
require lib/ffi-abi.f

create FFI-T-OUT 1 cells allot
create FFI-T-KP-CELL 1 cells allot

: FFI-T-STORE-X1 ( -- n ) cp@ {: fn:n :}
   $F9000001 fn patch32
   $D65F03C0 fn $4 + patch32
   fn ;

: FFI-T-KPARAM-SUM2 ( -- n ) cp@ {: fn:n :}
   $F9400009 fn       patch32
   $F940040A fn $4 +  patch32
   $F9400129 fn $8 +  patch32
   $F940014A fn $C +  patch32
   $8B0A0120 fn $10 + patch32
   $D65F03C0 fn $14 + patch32
   fn ;

: FFI-T-OUT-PARAM ( -- )
   0 FFI-T-OUT FFI-OUT!
   FFI-T-OUT 0 FFI-PTR-ARG!
   99 1 FFI-ARG!
   2 FFI-T-STORE-X1 FFI-CALLN drop
   FFI-T-OUT FFI-OUT@ 99 T= ;

: FFI-T-KPARAM-CAP ( -- )
   FFI-KPARAM-RESET
   [: 17 0 do i FFI-KPARAM-N+ loop ;] E-FFI-ARITY TTHROWSQ ;

\ Regression: the evaluate throw-unwind cell (EVALREC-CELL, src/habu/layout.f) must
\ live outside this file's FFI buffer block [FFI-BUF-OFF, FFI-KPARAM#-OFF+cell). An
\ FFI call fills that block, so an overlap silently clobbers the branch target and a
\ throw crossing an evaluate boundary (any FFI-using program run under include) jumps
\ to a data address. Assert the two regions are disjoint at build time.
: FFI-T-EVALREC-DISJOINT ( -- )
   EVALREC-CELL FFI-BUF-OFF <
   EVALREC-CELL FFI-KPARAM#-OFF CELL + >= or TTRUE ;

: FFI-T-KPARAMS ( -- )
   FFI-KPARAM-RESET
   13 FFI-KPARAM-N+
   21 FFI-T-KP-CELL FFI-OUT!
   FFI-T-KP-CELL FFI-KPARAM+
   FFI-KPARAM-COUNT 2 T=
   FFI-KPARAMS>N FFI-T-KPARAM-SUM2 CALL1 34 T=
   FFI-T-KPARAM-CAP
   FFI-KPARAM-RESET
   FFI-KPARAM-COUNT 0 T= ;

: FFI-ABI-RUN ( -- )
   T-RESET
   FFI-T-EVALREC-DISJOINT
   FFI-T-OUT-PARAM
   FFI-T-KPARAMS
   T-REPORT ;

FFI-ABI-RUN
