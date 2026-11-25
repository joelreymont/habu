\ engine-stack-lifecycle-lib.f - the active stack allocation follows stack
\ switches and nonlocal frame restoration.
require src/habu/stack-abi.f
require src/lib/error.f
require src/lib/test.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package STACK-LIFECYCLE-TEST

$1000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUTLEN
variable ERRLEN
variable STATUS

: CHILD-CAPTURE ( str -- ) {: src:str :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ src OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE PCAP-RESULT:>N STATUS ! ERRLEN ! OUTLEN ! ;

\ A child's output past IO-CAP fails its case under its label: uncaught, the
\ throw would end the file without naming the case.
: CHILD-TRUNCATED ( -- n )
   T-NEXT s" child output exceeded the capture" T-ASSERT-DETAIL
   0 OUTLEN ! 0 ERRLEN ! -1 ;

: CHILD-RC ( str -- n ) {: src:str :}
   [: src CHILD-CAPTURE ;] catch {: code:n :}
   code 0= if STATUS @ exit then
   code E-PROC-STDOUT-CAPACITY = if CHILD-TRUNCATED exit then
   code E-PROC-STDERR-CAPACITY = if CHILD-TRUNCATED exit then
   code throw ;

: PREFIX? ( str str -- bool ) {: g:str w:str :}
   g STR:LEN w STR:LEN < if 0 0= 0= exit then
   g STR:DATA w STR:LEN >str w T-STR= ;

\ GOT need only start with WANT: the guard-page record's first line names the
\ stack that faulted, and the word its note names varies.
: T-PREFIX= ( str str -- ) {: g:str w:str :}
   T-NEXT
   g w PREFIX? 0= if
      T-FAIL
      s" assert: expected prefix:" type cr w type cr
      s" got string:" type cr g type cr
   then
   T-LABEL-CLEAR ;

\ A push past the capacity and a return/loop frame past its region both exit
\ 102; only the message says which, so every call
\ site names the stack it expects.
: REFUSED-DATA ( str -- )
   CHILD-RC 102 T=
   ERR ERRLEN @ >str S\" error[E-STACK-BOUNDS]: the data stack ran past its bounds\n"
   T-PREFIX= ;

: REFUSED-RETURN ( str -- )
   CHILD-RC 102 T=
   ERR ERRLEN @ >str S\" error[E-STACK-BOUNDS]: the return stack ran past its bounds\n"
   T-PREFIX= ;

: REFUSED-LOOP ( str -- )
   CHILD-RC 102 T=
   ERR ERRLEN @ >str S\" error[E-STACK-BOUNDS]: the loop stack ran past its bounds\n"
   T-PREFIX= ;

\ An included file runs on a stack of its own. The in-process case sees the
\ caller's cells and depth after the throw; the children fill the stack the
\ unwind restored: FILL n holds n + 1 cells at its deepest ([n .. 0]).
: EVAL-CROSS-THROW ( -- )
   s" test/engine-stack-cross-throw.f" included ;

: CROSS-THROW ( -- n n n n )
   depth {: d0:n :}
   111 222 ['] EVAL-CROSS-THROW catch depth d0 - ;

: IN-PROCESS ( -- )
   s" evaluate unwind restores the caller's stack before catch" T-LABEL
   CROSS-THROW 3 T= 19 T= 222 T= 111 T=
   s" after the unwind a fill to the boot stack's capacity succeeds" T-LABEL
   S\" : X ( -- ) s\" test/engine-stack-cross-throw.f\" included ; : FILL ( n -- ) dup 0= if drop exit then dup 1 - recurse drop ; : GO ( -- ) ['] X catch drop STACK-ABI:BOOT-BYTES 8 / 1 - FILL ; GO"
   CHILD-RC 0 T=
   s" and one cell more overflows the data stack" T-LABEL
   S\" : X ( -- ) s\" test/engine-stack-cross-throw.f\" included ; : FILL ( n -- ) dup 0= if drop exit then dup 1 - recurse drop ; : GO ( -- ) ['] X catch drop STACK-ABI:BOOT-BYTES 8 / FILL ; GO"
   REFUSED-DATA ;

\ A data request below the base is the one rejection with a name: the checker
\ rejects a bare call on a shallower stack before the body runs, names the token
\ and exits 70.
: NAMED-UNDERDEPTH ( str str -- ) {: src:str diag:str :}
   src CHILD-RC 70 T=
   ERR ERRLEN @ >str diag STR:CONTAINS? TTRUE ;

\ Tier-0 2>r/2r> use the guarded VM return mapping. EDGE n fills n slots, one
\ `>r` a level, moves a pair at the top and checks each level's cell on the way
\ out; a read below the cells a definition pushed is rejected E-RETURN-STACK.
: PRIMITIVE-RETURN ( -- )
   s" last two return-stack slots" T-LABEL
   s" : EDGE ( n -- bool ) dup 0= if drop 11 22 2>r 2r> . . 0 0= exit then dup dup >r 1 - recurse swap r> = and ; STACK-ABI:RETURN-CELLS 2 - EDGE bool>n ."
   CHILD-RC 0 T=
   OUT OUTLEN @ >str S\" 22\n11\n-1\n" T$=
   s" whole return transfer needs two free slots" T-LABEL
   s" : EDGE ( n -- ) dup 0= if drop 11 22 2>r 2r> 2drop exit then 1 >r 1 - recurse r> drop ; STACK-ABI:RETURN-CELLS 1 - EDGE" REFUSED-RETURN ;

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
