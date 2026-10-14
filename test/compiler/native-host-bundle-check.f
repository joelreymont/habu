\ Exercise the newly installed target checker while the suite's one compiler
\ source bundle is loading. The temporary target definition is forgotten before
\ WINDOW-CLOSE, so no test definition enters any emitted engine image.
require src/lib/test.f
require src/compiler/native/host.f
require tools/native-build-core.f

package NATIVE-BUILD
private

: BUNDLE-HANDLE ( ptr u8 n -- n n )
   XREF-FIND DEF-OCC:SELECT ;

: BUNDLE-TARGET-ENTRY ( -- n )
   s" NATIVE-RUNTIME:BUNDLE-HOST-ABS" XREF-FIND XREF-START ;

CAST: TARGET-PATH-XT ( n -- [ ptr u8 n -- ] )
CAST: TARGET-CELL-XT ( n -- [ -- ptr n ] )

: TARGET-CELL@ ( n -- n )
   TARGET-CELL-XT execute @ ;

: TARGET-CELL! ( n n -- )
   {: value:n xt:n :}
   value xt TARGET-CELL-XT execute ! ;

: TARGET-LIVE-XT ( -- n )
   s" CODE-RECLAIM" s" LIVE-HI" OPEN-PRIVATE-XT ;

: TARGET-FLOOR-XT ( -- n )
   s" CODE-RECLAIM" s" FLOOR-A" OPEN-PRIVATE-XT ;

: BUNDLE-HOST-CHECK ( -- )
   T-RESET
   \ This is compiled by the target compiler installed at the end of
   \ native-runtime.f, while `abs` belongs to the retained host producer.
   s" package NATIVE-RUNTIME public : BUNDLE-HOST-MARK ( -- ) ; : BUNDLE-HOST-ABS ( n -- n ) abs ; ;package"
      evaluate-closed
   s" fresh target facts associate with a retained host implementation" T-LABEL
   BUNDLE-TARGET-ENTRY AOT-ARM:B0 @ >= TTRUE
   BUNDLE-TARGET-ENTRY cp@ < TTRUE
   BUNDLE-TARGET-ENTRY NHOST:ID-OF 0 T=
   s" abs" XREF-FIND XREF-START NHOST:ID-OF 0 > TTRUE
   s" NATIVE-RUNTIME:BUNDLE-HOST-ABS" BUNDLE-HANDLE
   s" abs" BUNDLE-HANDLE NHOST:ASSOCIATE
   \ Target-owned FORGET writes numeric address scratch outside the retired
   \ code span. Preserve its prior capture state around this test-only cut.
   s" XREF-FORGET-CP" OPEN-TARGET-XT TARGET-CELL@ {: saved-cp:n :}
   TARGET-LIVE-XT TARGET-CELL@ {: saved-live:n :}
   TARGET-FLOOR-XT TARGET-CELL@ {: saved-floor:n :}
   s" NATIVE-RUNTIME:BUNDLE-HOST-MARK"
      s" FORGET-DEFS-FROM" OPEN-TARGET-XT TARGET-PATH-XT execute
   saved-cp s" XREF-FORGET-CP" OPEN-TARGET-XT TARGET-CELL!
   saved-live TARGET-LIVE-XT TARGET-CELL!
   saved-floor TARGET-FLOOR-XT TARGET-CELL!
   s" NATIVE-RUNTIME:BUNDLE-HOST-ABS" XREF-FIND XREF-FOUND? 0= TTRUE
   T-REPORT ;

public

: RUN-BUNDLE-HOST-CHECK ( ptr u8 n -- )
   BUILD-TARGET:IDLE-CK
   DEFAULT-SOURCE-POLICY
   ['] BUNDLE-HOST-CHECK SOURCE-TAIL!
   RUN-BUNDLE-READY ;

;package
