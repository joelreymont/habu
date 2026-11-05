\ Measure bin/hb: its budget rows sum to its file length, and the walk refuses
\ an overlap, a gap or an overflowing range.
require src/lib/string.f
require src/lib/test.f
require tools/image-size-lib.f

package IMAGE-SIZE
private

: EST-SUM ( -- )
   s" the engine's budget rows sum to its file length" T-LABEL
   CLASS$ s" engine" T$=
   TOTAL-BYTES ILEN @ T=
   CODE-BYTES NAME-BYTES + DATA-WRITTEN + DATA-ZERO + PAD-BYTES + OTHER-BYTES +
   ILEN @ T= ;

: EST-OVERLAP ( -- )
   REC0 @ SITE0 ! -1 QUIET ! BUDGET ;

: EST-GAP ( -- )
   SITE0 @ 1+ SITE0 ! -1 QUIET ! BUDGET ;

: EST-OVERFLOW ( -- )
   $7FFFFFFFFFFFFFFF 1 ?RANGE ;

: EST-TILING ( -- )
   s" equal-size overlap and gap cannot pass the budget identity" T-LABEL
   SITE0 @ {: site:n :}
   [: EST-OVERLAP ;] E-ES-WALK TTHROWSQ  site SITE0 !
   [: EST-GAP ;] E-ES-WALK TTHROWSQ  site SITE0 !
   s" an overflowing range is refused before any image access" T-LABEL
   [: EST-OVERFLOW ;] E-ES-WALK TTHROWSQ
   0 QUIET ! ;

: EST-MAIN ( -- )
   T-RESET
   s" bin/hb" MEASURE
   EST-SUM
   EST-TILING
   T-REPORT ;

EST-MAIN
;package
