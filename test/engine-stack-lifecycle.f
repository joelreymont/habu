\ The active allocation follows stack switches and nonlocal frame restoration.
require src/habu/stack-abi.f
require lib/errors.f
require lib/memory.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package STACK-LIFECYCLE-TEST

\ Every VM stack the engine will run on is a guarded mapping (lib/memory.f
\ MEM-ALLOC-GUARDED): an inaccessible page on each side of the capacity, sized
\ in whole STACK-ABI:PAGE-BYTES multiples. POOL is that mapping, made once and
\ reused by every in-process case below; POOL-BYTES is its exact capacity.
PTR-VARIABLE POOL-A
STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED constant POOL-BYTES POOL-A !
: POOL ( -- ptr u8 ) POOL-A @ ;
PTR-VARIABLE SAVED-BASE
variable SAVED-CAP
PTR-VARIABLE SEEN-BASE
variable SEEN-CAP

: ACTIVE-BASE ( -- ptr u8 )
   data-base STACK-ABI:BASE-CELL + 0 ptr-field @ ;

: ACTIVE-CAP ( -- n )
   data-base STACK-ABI:CAP-CELL + @ ;

: SAVE-CALLER ( -- )
   ACTIVE-BASE SAVED-BASE !
   ACTIVE-CAP SAVED-CAP ! ;

: CALLER-RESTORED ( -- )
   ACTIVE-BASE SAVED-BASE @ = TTRUE
   ACTIVE-CAP SAVED-CAP @ T= ;

: OBSERVE ( -- )
   ACTIVE-BASE SEEN-BASE !
   ACTIVE-CAP SEEN-CAP ! ;

: EMPTY ( -- ) ;
: RAISE ( -- ) 19 throw ;
: CAP-ZERO ( -- ) ['] EMPTY POOL 0 run-in-stack ;

public
: CROSS-THROW ( -- )
   ['] RAISE POOL POOL-BYTES run-in-stack ;

private
: EVAL-CROSS-THROW ( -- )
   s" test/engine-stack-cross-throw.f" included ;

: EVAL-OBSERVE ( -- )
   s" test/engine-stack-evaluate-body.f" included OBSERVE ;

: IN-PROCESS ( -- )
   s" boot stack capacity" T-LABEL
   ACTIVE-CAP STACK-ABI:BOOT-BYTES T=
   SAVE-CALLER
   s" alternate allocation is active" T-LABEL
   ['] OBSERVE POOL POOL-BYTES run-in-stack
   SEEN-BASE @ POOL = TTRUE
   SEEN-CAP @ POOL-BYTES T=
   CALLER-RESTORED
   \ There is no such thing as a zero-capacity guarded mapping any more:
   \ MEM-ALLOC-GUARDED refuses a size that is not a whole STACK-ABI:PAGE-BYTES
   \ multiple, and run-in-stack's own GUARDED-EXTENT? proof (src/habu/habu1.f)
   \ refuses capacity 0 before the callback ever runs, catchable as
   \ E-STACK-UNGUARDED. The caller's own allocation is therefore never
   \ disturbed, because the stack switch never happened.
   s" capacity 0 is refused" T-LABEL
   ['] CAP-ZERO catch E-STACK-UNGUARDED T= CALLER-RESTORED
   s" catch restores allocation across run-in-stack" T-LABEL
   ['] CROSS-THROW catch 19 T= CALLER-RESTORED
   s" evaluate unwind restores allocation before catch" T-LABEL
   ['] EVAL-CROSS-THROW catch 19 T= CALLER-RESTORED
   s" clean evaluate preserves alternate allocation" T-LABEL
   ['] EVAL-OBSERVE POOL POOL-BYTES run-in-stack
   SEEN-BASE @ POOL = TTRUE
   SEEN-CAP @ POOL-BYTES T= CALLER-RESTORED ;

$200 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUTLEN
variable ERRLEN

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: CHILD-RC ( ptr u8 n -- n ) {: src:ptr size:n :}
   PROC-ARGV-RESET
   HB$ >LEN src size >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: out:len err:len :}
         out LEN>N OUTLEN ! err LEN>N ERRLEN ! 0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: out:len err:len code:rc :}
         out LEN>N OUTLEN ! err LEN>N ERRLEN ! code RC>N ENDOF
   ;MATCH ;

: PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: g:ptr gu:n w:ptr wu:n :}
   gu wu < if 0 0= 0= exit then
   g wu w wu T-STR= ;

\ Assert that GOT starts with WANT, rather than equals it whole: the guard-page
\ crash handler (src/habu/crash.f) writes a fixed "hb: stack bounds exceeded"
\ prefix and then names which stack faulted, so a caller that only cares about
\ the shared prefix -- or that also wants the specific stack name checked --
\ both go through this one comparison instead of a brittle exact match.
: T-PREFIX= ( ptr u8 n ptr u8 n -- ) {: g:ptr gu:n w:ptr wu:n :}
   T-NEXT
   g gu w wu PREFIX? 0= if
      T-FAIL
      s" assert: expected prefix:" type cr w wu type cr
      s" got string:" type cr g gu type cr
   then
   T-LABEL-CLEAR ;

\ A push past the capacity, and a return/loop frame past its region, both
\ still exit ENGINE-ERROR:STACK-BOUNDS (102); only the parenthetical after the
\ shared prefix says which, so every call site below names the specific stack
\ it expects. T-PREFIX= against the bare "hb: stack bounds exceeded" prefix
\ (no named helper wraps it -- every case here can name its stack) is still
\ available for a future case that cannot determine one. A malformed
\ run-in-stack descriptor no longer reaches this exit at all -- see
\ UNGUARDED-UNCAUGHT below.
: REFUSED-DATA ( ptr u8 n -- )
   CHILD-RC ENGINE-ERROR:STACK-BOUNDS T=
   ERR ERRLEN @ s" hb: stack bounds exceeded (data)" T-PREFIX= ;

: REFUSED-RETURN ( ptr u8 n -- )
   CHILD-RC ENGINE-ERROR:STACK-BOUNDS T=
   ERR ERRLEN @ s" hb: stack bounds exceeded (return)" T-PREFIX= ;

: REFUSED-LOOP ( ptr u8 n -- )
   CHILD-RC ENGINE-ERROR:STACK-BOUNDS T=
   ERR ERRLEN @ s" hb: stack bounds exceeded (loop)" T-PREFIX= ;

\ src/habu/layout.f UNCAUGHT-RC: the deterministic exit status for an uncaught
\ top-level throw (BTHROW THROW-NOREC), pinned here the same way
\ test/protection-span.f and test/runtime-regression-test.f already pin it.
67 constant CHILD-UNCAUGHT-RC

\ run-in-stack refuses an extent that is not a guarded mapping -- a
\ create/allot buffer, a null or unaligned base, a capacity that is zero or
\ not a STACK-ABI:PAGE-BYTES multiple, or a wrapping extent -- by throwing
\ E-STACK-UNGUARDED (-3802) before the callback ever runs (src/habu/habu1.f
\ BRUNSTACK GUARDED-EXTENT?). A child program that does not catch it dies
\ uncaught: "hb: uncaught throw code -3802" on fd 2, exit CHILD-UNCAUGHT-RC.
: UNGUARDED-REFUSED ( ptr u8 n -- )
   CHILD-RC CHILD-UNCAUGHT-RC T=
   ERR ERRLEN @ S\" hb: uncaught throw code -3802\n" T$= ;

\ A data request below the base is the one refusal with a name: the guard leaves
\ for the interpreter's E-UNDERFLOW diagnostic, which names the token and exits 70.
: NAMED-UNDERFLOW ( ptr u8 n ptr u8 n -- ) {: src:ptr size:n diag:ptr diagu:n :}
   src size CHILD-RC 70 T=
   ERR ERRLEN @ diag diagu T$= ;

\ GUARDED-EXTENT? (src/habu/habu1.f) runs at every run-in-stack switch, before
\ the callback and before the stack switch itself, and for a freshly entered
\ stack (used bytes = 0) it is strictly stronger than the descriptor check
\ that used to catch a malformed extent at the same place -- so strictly
\ stronger that the entry STACK-GUARD:CHECK-CURSOR call is gone: with
\ cursor == base, every branch that check emits was already proven impossible.
\ A malformed run-in-stack extent therefore never reaches the generic
\ "hb: stack bounds exceeded" exit any more; it is a catchable
\ E-STACK-UNGUARDED (-3802) throw.
\ test/stack-guard.f RUN-IN-STACK-REFUSALS owns one in-process case per clause
\ (null base, unaligned base, zero capacity, part-page capacity, wrapping
\ capacity, DATA-region base, create/allot buffer). The case here owns the
\ other half -- what the refusal looks like from outside a child that does not
\ catch it -- and pins the part no in-process case can show: the callback never
\ ran at all. Its body prints, and the child's stdout is empty.
: UNGUARDED-UNCAUGHT ( -- )
   s" an uncaught unguarded extent kills the child before its body runs" T-LABEL
   s" create BUF 32 allot : LOUD ( -- ) 42 . ; : GO ( -- ) ['] LOUD BUF 8 run-in-stack ; GO"
   UNGUARDED-REFUSED
   OUTLEN @ 0 T= ;

: PRIMITIVE-BOUNDARIES ( -- )
   s" last two return-stack slots" T-LABEL
   s" STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! 11 22 2>r 2r> . . data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 2 - = ."
   CHILD-RC 0 T=
   OUT OUTLEN @ S\" 22\n11\n-1\n" T$=
   s" whole return transfer needs two free slots" T-LABEL
   s" STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! 11 22 2>r" REFUSED-RETURN
   s" whole return transfer needs two live slots" T-LABEL
   s" 1 data-base RSP-CELL + ! 2r>" REFUSED-RETURN
   s" empty return-stack read" T-LABEL s" 2r>" REFUSED-RETURN
   s" empty data-stack adjustment" T-LABEL
   s" drop" S\" E-UNDERFLOW: drop\n" NAMED-UNDERFLOW
   s" the last loop frame" T-LABEL
   s" : NEST ( n -- ) dup 0= if drop exit then 1 0 do dup 1 - recurse loop drop ; STACK-ABI:LOOP-FRAMES NEST"
   CHILD-RC 0 T=
   s" one loop frame past the region" T-LABEL
   s" : NEST ( n -- ) dup 0= if drop exit then 1 0 do dup 1 - recurse loop drop ; STACK-ABI:LOOP-FRAMES 1 + NEST"
   REFUSED-LOOP ;

public
: RUN ( -- )
   T-RESET IN-PROCESS UNGUARDED-UNCAUGHT PRIMITIVE-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN
