\ A breakpoint may interrupt a valid guarded allocation.
require test/engine-stack-lifecycle.f
require lib/string.f

package STACK-LIFECYCLE-TEST

: DEBUGGER-LINES ( -- n )
   0 ERRLEN @ 0 ?do ERR i + c@ 10 = if 1+ then loop ;

: DEBUGGER-VALUES ( -- n )
   0 ERRLEN @ 16 - 0 max 0 ?do
      ERR i + 16 s" 0000000000000011" STR= if 1+ then
   loop ;

\ run-in-stack no longer accepts a capacity-0 mapping (GUARDED-EXTENT? refuses
\ it before the callback runs, test/engine-stack-lifecycle.f MALFORMED), so
\ both cases below run on a real 64 KB guarded stack; only the live depth at
\ the breakpoint -- not the allocation's capacity -- decides what the dump
\ shows.
: DEBUGGER-BOUNDARIES ( -- )
   s" breakpoint on a valid allocation has no top cell" T-LABEL
   s" 0 set-tier package SBP require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF : EMPTY ( -- ) ; ' EMPTY BP+ : GO ( -- ) ['] EMPTY BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ s" habu-bp:" CONTAINS? TTRUE
   ERR ERRLEN @ S\" habu-bp-stack:\n" ENDS-WITH? TTRUE
   DEBUGGER-LINES 5 T=
   s" breakpoint on a valid allocation preserves its value" T-LABEL
   s" 0 set-tier package SBP require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF : KEEP ( n -- n ) ; ' KEEP BP+ : ONE ( -- ) 17 KEEP drop ; : GO ( -- ) ['] ONE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ s" 0000000000000011" CONTAINS? TTRUE
   DEBUGGER-VALUES 2 T=
   DEBUGGER-LINES 7 T= ;

public
: DEBUGGER-RUN ( -- ) T-RESET DEBUGGER-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:DEBUGGER-RUN
