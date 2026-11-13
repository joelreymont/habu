\ A breakpoint may interrupt a valid guarded allocation.
require test/engine-stack-lifecycle-lib.f
require src/lib/string.f

package STACK-LIFECYCLE-TEST

: DEBUGGER-LINES ( -- n )
   0 ERRLEN @ 0 ?do ERR i + c@ 10 = if 1+ then loop ;

: DEBUGGER-HITS ( -- n )
   0 ERRLEN @ 8 - 0 max 0 ?do
      ERR i + 8 s" habu-bp:" STR= if 1+ then
   loop ;

: DEBUGGER-VALUES ( -- n )
   0 ERRLEN @ 16 - 0 max 0 ?do
      ERR i + 16 s" 0000000000000011" STR= if 1+ then
   loop ;

\ GO runs its word on `evaluate-closed`'s own stack, so only the live depth
\ above that stack's base at the breakpoint decides what the dump shows.
: DEBUGGER-BOUNDARIES ( -- )
   s" breakpoint on a valid allocation has no top cell" T-LABEL
   S\" require src/habu/debug.f package SBP : EMPTY ( -- ) ; ' EMPTY DEBUG:BP+ : GO ( -- ) s\" EMPTY\" evaluate-closed ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ s" habu-bp:" CONTAINS? TTRUE
   ERR ERRLEN @ S\" habu-bp-stack:\n" ENDS-WITH? TTRUE
   DEBUGGER-LINES 5 T=
   s" breakpoint on a valid allocation preserves its value" T-LABEL
   S\" require src/habu/debug.f package SBP : KEEP ( n -- n ) ; ' KEEP DEBUG:BP+ : ONE ( -- ) 17 KEEP drop ; : GO ( -- ) s\" ONE\" evaluate-closed ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ s" 0000000000000011" CONTAINS? TTRUE
   DEBUGGER-VALUES 2 T=
   DEBUGGER-LINES 7 T= ;

\ A PERSISTENT breakpoint keeps its BRK planted, so the word's entry instruction
\ never runs in place: the SIGTRAP handler emulates `str x30,[sp,#-16]!` and
\ runs any other entry in debug.f's trampoline, as T0's KEEP (`ret`) and TWICE
\ (`ldr x9,[x19,#-8]!`) are. The second call proves the BRK stays planted.
: PERSISTENT-BOUNDARIES ( -- )
   s" persistent breakpoint on a leaf word resumes it" T-LABEL
   s" require src/habu/debug.f package PBP : KEEP ( n -- n ) ; ' KEEP DEBUG:BP* : GO ( -- ) 17 KEEP . 25 KEEP . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ s" 17" CONTAINS? TTRUE
   OUT OUTLEN @ s" 25" CONTAINS? TTRUE
   DEBUGGER-HITS 2 T=
   s" persistent breakpoint on a calling word returns" T-LABEL
   s" require src/habu/debug.f package PBP : TWICE ( n -- n ) dup + ; ' TWICE DEBUG:BP* : GO ( -- ) 21 TWICE . 3 TWICE . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ s" 42" CONTAINS? TTRUE
   OUT OUTLEN @ s" 6" CONTAINS? TTRUE
   DEBUGGER-HITS 2 T= ;

public
: DEBUGGER-RUN ( -- ) T-RESET DEBUGGER-BOUNDARIES PERSISTENT-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:DEBUGGER-RUN
