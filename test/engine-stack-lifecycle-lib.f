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

: CHILD-RC ( ptr u8 n -- n ) {: src:ptr size:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN src size >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE PCAP-RESULT:>N >r ERRLEN ! OUTLEN ! r> ;

: PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: g:ptr gu:n w:ptr wu:n :}
   gu wu < if 0 0= 0= exit then
   g wu w wu T-STR= ;

\ GOT need only start with WANT: the guard-page crash handler (src/habu/crash.f)
\ names the stack that faulted, then the trapping code's lr.
: T-PREFIX= ( ptr u8 n ptr u8 n -- ) {: g:ptr gu:n w:ptr wu:n :}
   T-NEXT
   g gu w wu PREFIX? 0= if
      T-FAIL
      s" assert: expected prefix:" type cr w wu type cr
      s" got string:" type cr g gu type cr
   then
   T-LABEL-CLEAR ;

\ A push past the capacity and a return/loop frame past its region both exit
\ ENGINE-ERROR:STACK-BOUNDS (102); only the parenthetical after the shared
\ prefix says which, so every call site names the stack it expects.
: REFUSED-DATA ( ptr u8 n -- )
   CHILD-RC ENGINE-ERROR:STACK-BOUNDS T=
   ERR ERRLEN @ s" E-STACK-BOUNDS habu: stack bounds exceeded (data) lr " T-PREFIX= ;

: REFUSED-RETURN ( ptr u8 n -- )
   CHILD-RC ENGINE-ERROR:STACK-BOUNDS T=
   ERR ERRLEN @ s" E-STACK-BOUNDS habu: stack bounds exceeded (return) lr " T-PREFIX= ;

: REFUSED-LOOP ( ptr u8 n -- )
   CHILD-RC ENGINE-ERROR:STACK-BOUNDS T=
   ERR ERRLEN @ s" E-STACK-BOUNDS habu: stack bounds exceeded (loop) lr " T-PREFIX= ;

\ A data request below the base is the one refusal with a name: a primitive's
\ seed record states its minimum input depth, so the interpret band refuses a
\ shallower stack before the body runs, names the token and exits 70.
: NAMED-UNDERDEPTH ( ptr u8 n ptr u8 n -- ) {: src:ptr size:n diag:ptr diagu:n :}
   src size CHILD-RC 70 T=
   ERR ERRLEN @ diag diagu T$= ;

\ Tier-0 2>r/2r> use the guarded VM return mapping (src/habu/stack-abi.f). A raw
\ RSP store inside the compiled word sets each row's slots: the interpreter
\ parks its own cells on the return stack between top-level words.
: PRIMITIVE-RETURN-ARM ( -- )
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
   HB-TARGET-LINUX-X86-64? 0= if PRIMITIVE-RETURN-ARM then
   s" empty data-stack adjustment" T-LABEL
   s" drop" S\" E-UNDERFLOW habu: interpret stack underdepth: drop\n" NAMED-UNDERDEPTH
   s" the last loop frame" T-LABEL
   s" : NEST ( n -- ) dup 0= if drop exit then 1 0 do dup 1 - recurse loop drop ; STACK-ABI:LOOP-FRAMES NEST"
   CHILD-RC 0 T=
   s" one loop frame past the region" T-LABEL
   s" : NEST ( n -- ) dup 0= if drop exit then 1 0 do dup 1 - recurse loop drop ; STACK-ABI:LOOP-FRAMES 1 + NEST"
   \ x86-64's guard handler writes an `hb:` line with no code and no lr.
   HB-TARGET-LINUX-X86-64? if
      CHILD-RC ENGINE-ERROR:STACK-BOUNDS T=
      ERR ERRLEN @ S\" hb: stack bounds exceeded (loop)\n" T$=
   else REFUSED-LOOP then ;

;package
