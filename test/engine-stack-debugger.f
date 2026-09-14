\ A breakpoint may interrupt a valid empty alternate allocation.
require test/engine-stack-lifecycle.f
require lib/string.f

package STACK-LIFECYCLE-TEST

: DEBUGGER-LINES ( -- n )
   0 ERRLEN @ 0 ?do ERR i + c@ 10 = if 1+ then loop ;

: DEBUGGER-VALUES ( -- n )
   0 ERRLEN @ 16 - 0 max 0 ?do
      ERR i + 16 s" 0000000000000011" STR= if 1+ then
   loop ;

: DEBUGGER-BOUNDARIES ( -- )
   s" breakpoint on an empty allocation has no top cell" T-LABEL
   s" 0 set-tier package SBP create BUF 32 allot : EMPTY ( -- ) ; ' EMPTY BP+ : GO ( -- ) ['] EMPTY BUF 0 run-in-stack ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ s" habu-bp:" CONTAINS? TTRUE
   ERR ERRLEN @ S\" habu-bp-stack:\n" ENDS-WITH? TTRUE
   DEBUGGER-LINES 5 T=
   s" breakpoint at the exact cell limit preserves its value" T-LABEL
   s" 0 set-tier package SBP create BUF 32 allot : KEEP ( n -- n ) ; ' KEEP BP+ : ONE ( -- ) 17 KEEP drop ; : GO ( -- ) ['] ONE BUF 8 run-in-stack ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ s" 0000000000000011" CONTAINS? TTRUE
   DEBUGGER-VALUES 2 T=
   DEBUGGER-LINES 7 T= ;

public
: DEBUGGER-RUN ( -- ) T-RESET DEBUGGER-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:DEBUGGER-RUN
