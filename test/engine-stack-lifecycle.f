\ The active allocation follows stack switches and nonlocal frame restoration.
require src/habu/stack-abi.f
require src/habu/xref.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package STACK-LIFECYCLE-TEST

$800 constant POOL-BYTES
create POOL POOL-BYTES allot
variable SAVED-BASE
variable SAVED-CAP
variable SEEN-BASE
variable SEEN-CAP

: ACTIVE-BASE ( -- ptr u8 )
   data-base STACK-ABI:BASE-CELL + 0 ptr-field @ ;

: ACTIVE-CAP ( -- n )
   data-base STACK-ABI:CAP-CELL + @ ;

: SAVE-CALLER ( -- )
   ACTIVE-BASE SAVED-BASE 0 ptr-field !
   ACTIVE-CAP SAVED-CAP ! ;

: CALLER-RESTORED ( -- )
   ACTIVE-BASE SAVED-BASE 0 ptr-field @ = TTRUE
   ACTIVE-CAP SAVED-CAP @ T= ;

: OBSERVE ( -- )
   ACTIVE-BASE SEEN-BASE 0 ptr-field !
   ACTIVE-CAP SEEN-CAP ! ;

: EMPTY ( -- ) ;
: RAISE ( -- ) 19 throw ;

: GUARD-ENTRY ( -- )
   s" getter names the current registered helper" T-LABEL
   s" (STACK-DATA)" OWNER-API-PRI-WID XREF-FIND-WL {: rec:ptr :}
   rec XREF-FOUND? dup TTRUE 0= if exit then
   stack-data-entry rec XREF-START T= ;

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
   SEEN-BASE 0 ptr-field @ POOL = TTRUE
   SEEN-CAP @ POOL-BYTES T=
   CALLER-RESTORED
   s" an empty allocation permits an empty callback" T-LABEL
   ['] EMPTY POOL 0 run-in-stack CALLER-RESTORED
   s" catch restores allocation across run-in-stack" T-LABEL
   ['] CROSS-THROW catch 19 T= CALLER-RESTORED
   s" evaluate unwind restores allocation before catch" T-LABEL
   ['] EVAL-CROSS-THROW catch 19 T= CALLER-RESTORED
   s" clean evaluate preserves alternate allocation" T-LABEL
   ['] EVAL-OBSERVE POOL POOL-BYTES run-in-stack
   SEEN-BASE 0 ptr-field @ POOL = TTRUE
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

: REFUSED ( ptr u8 n -- )
   CHILD-RC ENGINE-ERROR:STACK-BOUNDS T=
   ERR ERRLEN @ s" hb: stack bounds exceeded" T$= ;

\ A data request below the base is the one refusal with a name: the guard leaves
\ for the interpreter's E-UNDERFLOW diagnostic, which names the token and exits 70.
: NAMED-UNDERFLOW ( ptr u8 n ptr u8 n -- ) {: src:ptr size:n diag:ptr diagu:n :}
   src size CHILD-RC 70 T=
   ERR ERRLEN @ diag diagu T$= ;

: MALFORMED ( -- )
   s" null stack base" T-LABEL
   s" : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY NULL-PTR 0 run-in-stack ; GO" REFUSED
   s" unaligned stack base" T-LABEL
   s" create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF 1 + 32 run-in-stack ; GO" REFUSED
   s" wrapping stack descriptor" T-LABEL
   s" create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF -1 run-in-stack ; GO" REFUSED ;

: PRIMITIVE-BOUNDARIES ( -- )
   s" last two return-stack slots" T-LABEL
   s" STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! 11 22 2>r 2r> . . data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 2 - = ."
   CHILD-RC 0 T=
   OUT OUTLEN @ S\" 22\n11\n-1\n" T$=
   s" whole return transfer needs two free slots" T-LABEL
   s" STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! 11 22 2>r" REFUSED
   s" whole return transfer needs two live slots" T-LABEL
   s" 1 data-base RSP-CELL + ! 2r>" REFUSED
   s" empty return-stack read" T-LABEL s" 2r>" REFUSED
   s" empty data-stack adjustment" T-LABEL
   s" drop" S\" E-UNDERFLOW: drop\n" NAMED-UNDERFLOW
   s" the last loop frame" T-LABEL
   s" : NEST ( n -- ) dup 0= if drop exit then 1 0 do dup 1 - recurse loop drop ; STACK-ABI:LOOP-FRAMES NEST"
   CHILD-RC 0 T=
   s" one loop frame past the region" T-LABEL
   s" : NEST ( n -- ) dup 0= if drop exit then 1 0 do dup 1 - recurse loop drop ; STACK-ABI:LOOP-FRAMES 1 + NEST"
   REFUSED ;

public
: RUN ( -- )
   T-RESET GUARD-ENTRY IN-PROCESS MALFORMED PRIMITIVE-BOUNDARIES T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN
