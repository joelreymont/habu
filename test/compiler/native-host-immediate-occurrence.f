\ A preflight may retire and re-expose the selected record. Native immediate
\ dispatch must refuse that occurrence before the body's write executes.
1 set-tier
require lib/test.f
require src/habu/xref.f

package NATIVE-HOST-IMMEDIATE
private

variable RAN
variable CUT
variable COUNT
variable PRIOR
variable PRIOR-HOOK

: RETIRE ( ptr u8 n ptr u8 n n -- )
   drop 2drop 2drop
   CUT @ ndict!
   COUNT @ ndict! ;

: TRIGGER ( -- )
   s" : USER ( -- n ) RETIRED 51 ;" evaluate-closed ;

TRUSTED: CHECK ( -- )
   s" NATIVE-HOST-IMMEDIATE:RETIRED" XREF-FIND DEF-OCC:SELECT
   drop CUT !
   ndict@ COUNT !
   data-base COMPILE-PREFLIGHT-CELL + @ PRIOR !
   data-base HOOK-CELL + @ PRIOR-HOOK !
   0 set-check
   PRIOR-HOOK @ set-check
   ['] RETIRE set-preflight
   [: TRIGGER ;] catch
   0 set-check
   PRIOR-HOOK @ set-check
   PRIOR @ set-preflight
   DEF-OCC:E-STALE T=
   RAN @ 0 T= ;

public

: RETIRED ( -- ) 1 RAN ! ; immediate
s" NATIVE-HOST-IMMEDIATE:RETIRED" 0 parse-imm

T-RESET
s" immediate preserves occurrence across preflight" T-LABEL
CHECK
T-REPORT

;package
