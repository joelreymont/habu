\ Live registry membership follows allocation, including precompiled reserve
\ calls after cleanup. Range cleanup keeps writer buffers outside the value.
require lib/test.f

package DYNAMIC-BUFFER-REGISTRY-TEST
private

variable BASE-N
DYNAMIC-STORAGE:REGISTERED-N BASE-N !

DYNAMIC-BUFFER OUTSIDE n
create WINDOW-START
DYNAMIC-BUFFER FIRST n
DYNAMIC-BUFFER SECOND n
create WINDOW-END
DYNAMIC-BUFFER LAST n

: LIVE= ( n -- )
   BASE-N @ + DYNAMIC-STORAGE:REGISTERED-N swap T= ;

: FIRST-EMPTY ( -- ) 0 FIRST drop ;
: SECOND-EMPTY ( -- ) 0 SECOND drop ;
: BAD-SIZE ( -- ) -1 FIRST-RESERVE ;
: BAD-MAP ( -- ) $7FFFFFFFFFFFFFFF CELL / FIRST-RESERVE ;

: WINDOW-CLEAN ( -- )
   WINDOW-START byte-view WINDOW-END WINDOW-START - DYNAMIC-STORAGE:RELEASE-RANGE ;

: CUT-CONTROL ( -- )
   WINDOW-START byte-view 1 DYNAMIC-STORAGE:RELEASE-RANGE ;

: RUN ( -- )
   T-RESET
   s" declarations and zero reserves have no membership" T-LABEL
   0 LIVE=
   0 FIRST-RESERVE
   FIRST-RELEASE FIRST-RELEASE
   0 LIVE=

   s" failed first allocation publishes no control or membership" T-LABEL
   ['] BAD-SIZE 7121 TTHROWS
   ['] BAD-MAP 7138 TTHROWS
   0 LIVE=
   ['] FIRST-EMPTY 7122 TTHROWS

   s" growth preserves data and one membership" T-LABEL
   1 FIRST-RESERVE
   17 0 FIRST !
   1024 FIRST-RESERVE
   1 LIVE=
   0 FIRST @ 17 T=
   ['] BAD-MAP 7138 TTHROWS
   1 LIVE=
   0 FIRST @ 17 T=

   s" release repairs the handle of a moved registry entry" T-LABEL
   1 SECOND-RESERVE
   1 LAST-RESERVE
   3 LIVE=
   FIRST-RELEASE
   2 LIVE=
   LAST-RELEASE
   1 LIVE=
   SECOND-RELEASE
   0 LIVE=

   s" range cleanup preserves outside writer storage" T-LABEL
   1 OUTSIDE-RESERVE 91 0 OUTSIDE !
   1 FIRST-RESERVE 1 SECOND-RESERVE
   1 LAST-RESERVE 92 0 LAST !
   4 LIVE=
   ['] CUT-CONTROL 7121 TTHROWS
   4 LIVE=
   WINDOW-CLEAN
   2 LIVE=
   0 OUTSIDE @ 91 T=
   0 LAST @ 92 T=
   ['] FIRST-EMPTY 7122 TTHROWS
   ['] SECOND-EMPTY 7122 TTHROWS
   WINDOW-CLEAN
   2 LIVE=

   s" precompiled reserve rejoins each later capture" T-LABEL
   1 FIRST-RESERVE 33 0 FIRST !
   3 LIVE=
   WINDOW-CLEAN
   2 LIVE=
   1 FIRST-RESERVE 34 0 FIRST !
   0 FIRST @ 34 T=
   WINDOW-CLEAN
   2 LIVE=
   OUTSIDE-RELEASE LAST-RELEASE
   0 LIVE=

   s" full cleanup is empty, repeatable and permits later reserve" T-LABEL
   DYNAMIC-STORAGE:RELEASE-ALL
   DYNAMIC-STORAGE:REGISTERED-N 0 T=
   DYNAMIC-STORAGE:RELEASE-ALL
   1 FIRST-RESERVE
   DYNAMIC-STORAGE:REGISTERED-N 1 T=
   DYNAMIC-STORAGE:RELEASE-ALL
   DYNAMIC-STORAGE:REGISTERED-N 0 T=
   ['] FIRST-EMPTY 7122 TTHROWS
   T-REPORT ;

RUN
;package
