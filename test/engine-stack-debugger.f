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

\ run-in-stack refuses a capacity-0 mapping (GUARDED-EXTENT?, test/stack-guard.f
\ CAPACITY-ZERO-REFUSAL), so both cases run on a real 64 KB guarded stack: only
\ the live depth at the breakpoint decides what the dump shows.
: DEBUGGER-BOUNDARIES ( -- )
   s" breakpoint on a valid allocation has no top cell" T-LABEL
   s" 0 set-tier require src/habu/debug.f package SBP require src/lib/memory.f PTR-VARIABLE BUF-A STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop BUF-A ! : BUF ( -- ptr u8 ) BUF-A @ ; : EMPTY ( -- ) ; ' EMPTY DEBUG:BP+ : GO ( -- ) ['] EMPTY BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ s" habu-bp:" CONTAINS? TTRUE
   ERR ERRLEN @ S\" habu-bp-stack:\n" ENDS-WITH? TTRUE
   DEBUGGER-LINES 5 T=
   s" breakpoint on a valid allocation preserves its value" T-LABEL
   s" 0 set-tier require src/habu/debug.f package SBP require src/lib/memory.f PTR-VARIABLE BUF-A STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop BUF-A ! : BUF ( -- ptr u8 ) BUF-A @ ; : KEEP ( n -- n ) ; ' KEEP DEBUG:BP+ : ONE ( -- ) 17 KEEP drop ; : GO ( -- ) ['] ONE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO ;package"
   CHILD-RC 0 T=
   OUTLEN @ 0 T=
   ERR ERRLEN @ s" 0000000000000011" CONTAINS? TTRUE
   DEBUGGER-VALUES 2 T=
   DEBUGGER-LINES 7 T= ;

\ A PERSISTENT breakpoint keeps its BRK planted, so the word's entry instruction
\ never runs and the SIGTRAP handler emulates it (src/habu/habu2.f
\ C-BP-EMULATE). A tier-0 entry is one of two instructions, one case each: a
\ leaf's nop (EM-COMPILE-RET rewrote it) and a caller's `str x30,[sp,#-16]!`.
\ The printed 42 is the return value through the emulated entry: a handler that
\ failed to store x30 at the lowered sp would return through whatever was there.
\ The second call proves the BRK stays planted, the reason the handler emulates.
\ Any other first instruction runs out of line, in the slot's trampoline: tier-1
\ leaf TW begins with `ldur`, the same at any address, so its breakpoint stays
\ persistent and fires on both calls; T2 begins with `b` to TW, which would
\ branch from the wrong place, so its breakpoint degrades to one-shot and fires
\ once. Both return the right values.
: PERSISTENT-OUT-OF-LINE ( -- )
   s" persistent breakpoint runs a leaf's first instruction out of line" T-LABEL
   s" 1 set-tier require src/habu/debug.f package PBP : TW ( n -- n ) dup + ; ' TW DEBUG:BP* : GO ( -- ) 21 TW . 3 TW . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ s" 42" CONTAINS? TTRUE
   OUT OUTLEN @ s" 6" CONTAINS? TTRUE
   DEBUGGER-HITS 2 T=
   s" persistent breakpoint on a PC-relative first instruction degrades" T-LABEL
   s" 1 set-tier require src/habu/debug.f package PBP : TW ( n -- n ) dup + ; : T2 ( n -- n ) TW ; ' T2 DEBUG:BP* : GO ( -- ) 21 T2 . 3 T2 . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ s" 42" CONTAINS? TTRUE
   OUT OUTLEN @ s" 6" CONTAINS? TTRUE
   DEBUGGER-HITS 1 T= ;

: PERSISTENT-BOUNDARIES ( -- )
   s" persistent breakpoint on a leaf word resumes it" T-LABEL
   s" 0 set-tier require src/habu/debug.f package PBP : KEEP ( n -- n ) ; ' KEEP DEBUG:BP* : GO ( -- ) 17 KEEP . 25 KEEP . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ s" 17" CONTAINS? TTRUE
   OUT OUTLEN @ s" 25" CONTAINS? TTRUE
   DEBUGGER-HITS 2 T=
   s" persistent breakpoint on a calling word returns" T-LABEL
   s" 0 set-tier require src/habu/debug.f package PBP : TWICE ( n -- n ) dup + ; ' TWICE DEBUG:BP* : GO ( -- ) 21 TWICE . 3 TWICE . ; GO ;package"
   CHILD-RC 0 T=
   OUT OUTLEN @ s" 42" CONTAINS? TTRUE
   OUT OUTLEN @ s" 6" CONTAINS? TTRUE
   DEBUGGER-HITS 2 T=
   PERSISTENT-OUT-OF-LINE ;

public
: DEBUGGER-RUN ( -- ) T-RESET DEBUGGER-BOUNDARIES PERSISTENT-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:DEBUGGER-RUN
