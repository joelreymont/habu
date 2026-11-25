\ A breakpoint may interrupt a valid guarded allocation.
require test/engine-stack-lifecycle-lib.f
require src/lib/string.f

package STACK-LIFECYCLE-TEST

: DEBUGGER-LINES ( -- n )
   0 ERRLEN @ 0 ?do ERR i + c@ 10 = if 1+ then loop ;

: DEBUGGER-HITS ( -- n )
   0 ERRLEN @ 8 - 0 max 0 ?do
      ERR i + 8 >str s" habu-bp:" STR:= if 1+ then
   loop ;

: DEBUGGER-VALUES ( -- n )
   0 ERRLEN @ 16 - 0 max 0 ?do
      ERR i + 16 >str s" 0000000000000011" STR:= if 1+ then
   loop ;

\ GO runs its word on `evaluate-closed`'s own stack, so only the live depth
\ above that stack's base at the breakpoint decides what the dump shows.
: DEBUGGER-BOUNDARIES ( -- )
   s" breakpoint on a valid allocation has no top cell" T-LABEL
   S\" require src/habu/debug.f package SBP : EMPTY ( -- ) ; ' EMPTY DEBUG:BP+ : GO ( -- ) s\" EMPTY\" evaluate-closed ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ >str s" habu-bp:" STR:CONTAINS? TTRUE
   ERR ERRLEN @ >str S\" habu-bp-stack:\n" STR:ENDS-WITH? TTRUE
   DEBUGGER-LINES 5 T=
   s" breakpoint on a valid allocation preserves its value" T-LABEL
   S\" require src/habu/debug.f package SBP : KEEP ( n -- n ) ; ' KEEP DEBUG:BP+ : ONE ( -- ) 17 KEEP drop ; : GO ( -- ) s\" ONE\" evaluate-closed ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ >str s" 0000000000000011" STR:CONTAINS? TTRUE
   DEBUGGER-VALUES 2 T=
   DEBUGGER-LINES 7 T= ;

\ T0 enters KEEP with `ret`, TWICE with `ldr x9,[x19,#-8]!` and CALLS with
\ `str x30,[sp,#-16]!`; the planted BRK displaces each on every call.
: PERSISTENT-BOUNDARIES ( -- )
   s" persistent breakpoint on a leaf word resumes it" T-LABEL
   s" require src/habu/debug.f package PBP : KEEP ( n -- n ) ; ' KEEP DEBUG:BP* : GO ( -- ) 17 KEEP . 25 KEEP . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ >str s" 17" STR:CONTAINS? TTRUE
   OUT OUTLEN @ >str s" 25" STR:CONTAINS? TTRUE
   DEBUGGER-HITS 2 T=
   s" persistent breakpoint runs an entry out of line and resumes" T-LABEL
   s" require src/habu/debug.f package PBP : TWICE ( n -- n ) dup + ; ' TWICE DEBUG:BP* : GO ( -- ) 21 TWICE . 3 TWICE . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ >str s" 42" STR:CONTAINS? TTRUE
   OUT OUTLEN @ >str s" 6" STR:CONTAINS? TTRUE
   DEBUGGER-HITS 2 T=
   s" persistent breakpoint on a calling word returns" T-LABEL
   s" require src/habu/debug.f package PBP : INC ( n -- n ) 1+ ; : CALLS ( n -- n ) INC 2 * ; ' CALLS DEBUG:BP* : GO ( -- ) 20 CALLS . 4 CALLS . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ >str s" 42" STR:CONTAINS? TTRUE
   OUT OUTLEN @ >str s" 10" STR:CONTAINS? TTRUE
   DEBUGGER-HITS 2 T= ;

public
: DEBUGGER-RUN ( -- ) T-RESET DEBUGGER-BOUNDARIES PERSISTENT-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:DEBUGGER-RUN
