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
\ it before the callback runs -- test/stack-guard.f CAPACITY-ZERO-REFUSAL), so
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

\ A PERSISTENT breakpoint keeps its BRK planted, so the word's entry instruction
\ never runs and the SIGTRAP handler has to be it (src/habu/habu2.f
\ C-BP-EMULATE). A tier-0 entry is one of exactly two instructions, and the two
\ cases below are one of each: a leaf's slot holds the nop EM-COMPILE-RET
\ rewrote it to, and a word that calls holds `str x30,[sp,#-16]!`.
\
\ WHAT EACH CASE PROVES. The answer printed on stdout is the word's RETURN
\ VALUE, reached through the emulated entry: if the handler failed to store the
\ interrupted x30 at the lowered sp, the epilogue's `ldr x30,[sp],#16` would
\ return through whatever was there and the child would not print 42 and exit 0.
\ Calling the word twice also proves the breakpoint really is persistent -- the
\ BRK is still planted for the second call -- which is the whole reason the
\ handler emulates instead of restoring.
: PERSISTENT-BOUNDARIES ( -- )
   s" persistent breakpoint on a leaf word resumes it" T-LABEL
   s" 0 set-tier package PBP : KEEP ( n -- n ) ; ' KEEP BP* : GO ( -- ) 17 KEEP . 25 KEEP . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ s" 17" CONTAINS? TTRUE
   OUT OUTLEN @ s" 25" CONTAINS? TTRUE
   ERR ERRLEN @ s" habu-bp:" CONTAINS? TTRUE
   s" persistent breakpoint on a calling word returns" T-LABEL
   s" 0 set-tier package PBP : TWICE ( n -- n ) dup + ; ' TWICE BP* : GO ( -- ) 21 TWICE . 3 TWICE . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ s" 42" CONTAINS? TTRUE
   OUT OUTLEN @ s" 6" CONTAINS? TTRUE
   ERR ERRLEN @ s" habu-bp:" CONTAINS? TTRUE ;

public
: DEBUGGER-RUN ( -- ) T-RESET DEBUGGER-BOUNDARIES PERSISTENT-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:DEBUGGER-RUN
