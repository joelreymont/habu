split-str
\ engine-stack-lifecycle-lib.f - the active stack allocation follows stack
\ switches and nonlocal frame restoration.
require src/habu/stack-abi.f
require src/lib/error.f
require src/lib/test.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package STACK-LIFECYCLE-TEST

PTR-VARIABLE SAVED-BASE
variable SAVED-CAP

: ACTIVE-BASE ( -- ^u8 )
   data-base STACK-ABI:BASE-CELL + 0 ptr-field @ ;

: ACTIVE-CAP ( -- n )
   data-base STACK-ABI:CAP-CELL + @ ;

: SAVE-CALLER ( -- )
   ACTIVE-BASE SAVED-BASE !
   ACTIVE-CAP SAVED-CAP ! ;

: CALLER-RESTORED ( -- )
   ACTIVE-BASE SAVED-BASE @ = TTRUE
   ACTIVE-CAP SAVED-CAP @ T= ;

: EVAL-CROSS-THROW ( -- )
   s" test/engine-stack-cross-throw.f" included ;

: IN-PROCESS ( -- )
   s" boot stack capacity" T-LABEL
   ACTIVE-CAP STACK-ABI:BOOT-BYTES T=
   SAVE-CALLER
   s" evaluate unwind restores allocation before catch" T-LABEL
   ['] EVAL-CROSS-THROW catch 19 T= CALLER-RESTORED ;

$200 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUTLEN
variable ERRLEN
variable STATUS

: CHILD-CAPTURE ( str -- ) {: src:^ size:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN src size >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE PCAP-RESULT:>N STATUS ! ERRLEN ! OUTLEN ! ;

\ A crashed child's dump outgrows IO-CAP, and E-PROC-TRUNCATED uncaught would
\ end the file without naming the case. The case fails under its label, and the
\ empty capture and -1 fail the checks after it.
: CHILD-TRUNCATED ( -- n )
   T-NEXT s" child output exceeded the capture" T-ASSERT-DETAIL
   0 OUTLEN ! 0 ERRLEN ! -1 ;

: CHILD-RC ( str -- n ) {: src:^ size:n :}
   [: src size CHILD-CAPTURE ;] catch {: code:n :}
   code 0= if STATUS @ exit then
   code E-PROC-TRUNCATED = if CHILD-TRUNCATED exit then
   code throw ;

: PREFIX? ( str str -- bool ) {: g:^ gu:n w:^ wu:n :}
   gu wu < if 0 0= 0= exit then
   g wu w wu T-STR= ;

\ GOT need only start with WANT: the guard-page record's first line names the
\ stack that faulted, and the word its note names varies.
: T-PREFIX= ( str str -- ) {: g:^ gu:n w:^ wu:n :}
   T-NEXT
   g gu w wu PREFIX? 0= if
      T-FAIL
      s" assert: expected prefix:" type cr w wu type cr
      s" got string:" type cr g gu type cr
   then
   T-LABEL-CLEAR ;

\ A push past the capacity and a return/loop frame past its region both exit
\ 102; only the message says which, so every call
\ site names the stack it expects.
: REFUSED-DATA ( str -- )
   CHILD-RC 102 T=
   ERR ERRLEN @ S\" error[E-STACK-BOUNDS]: the data stack ran past its bounds\n"
   T-PREFIX= ;

: REFUSED-RETURN ( str -- )
   CHILD-RC 102 T=
   ERR ERRLEN @ S\" error[E-STACK-BOUNDS]: the return stack ran past its bounds\n"
   T-PREFIX= ;

: REFUSED-LOOP ( str -- )
   CHILD-RC 102 T=
   ERR ERRLEN @ S\" error[E-STACK-BOUNDS]: the loop stack ran past its bounds\n"
   T-PREFIX= ;

\ A data request below the base is the one rejection with a name: the checker
\ rejects a bare call on a shallower stack before the body runs, names the token
\ and exits 70.
: NAMED-UNDERDEPTH ( str str -- ) {: src:^ size:n diag:^ diagu:n :}
   src size CHILD-RC 70 T=
   ERR ERRLEN @ diag diagu CONTAINS? TTRUE ;

\ Tier-0 2>r/2r> use the guarded VM return mapping (src/habu/stack-abi.f). A raw
\ RSP store inside the compiled word sets each row's slots: the interpreter
\ parks its own cells on the return stack between top-level words.
: PRIMITIVE-RETURN ( -- )
   s" last two return-stack slots" T-LABEL
   s" variable OLD-RSP : EDGE ( -- ) data-base RSP-CELL + @ OLD-RSP ! STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! 11 22 2>r 2r> . . data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 2 - = bool>n . OLD-RSP @ data-base RSP-CELL + ! ; EDGE"
   CHILD-RC 0 T=
   OUT OUTLEN @ S\" 22\n11\n-1\n" T$=
   s" whole return transfer needs two free slots" T-LABEL
   s" : EDGE ( -- ) STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! 11 22 2>r 2r> 2drop ; EDGE" REFUSED-RETURN
   s" whole return transfer needs two live slots" T-LABEL
   s" : EDGE ( -- ) 11 22 2>r 1 data-base RSP-CELL + ! 2r> 2drop ; EDGE" REFUSED-RETURN
   s" empty return-stack read" T-LABEL
   s" : EDGE ( -- ) 11 22 2>r 0 data-base RSP-CELL + ! 2r> 2drop ; EDGE" REFUSED-RETURN ;

: PRIMITIVE-BOUNDARIES ( -- )
   PRIMITIVE-RETURN
   s" empty data-stack adjustment" T-LABEL
   s" drop" s" error[E-INPUT-UNDERFLOW]: input underflow: the statement takes more cells than the stack holds (needs 1, has 0)" NAMED-UNDERDEPTH
   s" the last loop frame" T-LABEL
   s" : NEST ( n -- ) dup 0= if drop exit then 1 0 do dup 1 - recurse loop drop ; STACK-ABI:LOOP-FRAMES NEST"
   CHILD-RC 0 T=
   s" one loop frame past the region" T-LABEL
   s" : NEST ( n -- ) dup 0= if drop exit then 1 0 do dup 1 - recurse loop drop ; STACK-ABI:LOOP-FRAMES 1 + NEST"
   REFUSED-LOOP ;

;package
