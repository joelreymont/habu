\ require-cap-test.f - the require inventory grows past 512 entries (the first
\ case) and through another growth step (the second). Each case forks a SUBJECT
\ child that resets the require registry, flips DISCOVERY so synthetic paths
\ never touch the filesystem (required then skips the load), and drives
\ `require` to a target count through the real loader's dedup scan and storage
\ path.

require src/lib/test.f
require src/lib/test/outcome.f       \ T-TIMED-OUT - the report of a child past its deadline
require src/lib/process.f            \ outcome sumtype for the child completion
require src/lib/test/subject.f       \ SUBJECT:RUN - isolated evaluation of the subject

package REQUIRE-CAP

$4000 constant FORGE-CAP
$800 constant IO-CAP
30000 constant TIMEOUT-MS

create FORGE-BUF FORGE-CAP allot
variable FORGE-U
create OUT-BUF IO-CAP allot
create ERR-BUF IO-CAP allot
variable ERR-U
variable RC-N
variable EXITED?

\ A child program that fills the require table to K entries.

: FORGE$ ( -- ptr u8 n )  FORGE-BUF FORGE-U @ ;

: FORGE-C ( n -- ) {: c:n :}
   FORGE-U @ 1+ FORGE-CAP > if s" require-cap: forge overflow" 1 die then
   c FORGE-BUF FORGE-U @ + c!
   FORGE-U @ 1+ FORGE-U ! ;

: FORGE-APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   FORGE-U @ u + FORGE-CAP > if s" require-cap: forge overflow" 1 die then
   a FORGE-BUF FORGE-U @ + u BYTE-COPY
   FORGE-U @ u + FORGE-U ! ;

: FORGE-LINE ( n -- ) {: i:n :}        \ one fresh require: "require p<a-z><a-z>"
   s" require p" FORGE-APPEND
   $61 i 26 / +   FORGE-C
   $61 i 26 mod + FORGE-C
   10 FORGE-C ;

\ After the last growth every row must still be found: requiring them all again
\ leaves the count alone, so each path and length survived the copies.
: FORGE-GEN ( n -- ptr u8 n ) {: k:n :}
   0 FORGE-U !
   s" 0 REQUIRE-REG:TRUNCATE 0 REQUIRE-BASE ! DISCOVERY-ON" FORGE-APPEND  10 FORGE-C
   0 begin dup k < while dup FORGE-LINE 1+ repeat drop
   S\" : SAME-COUNT ( n -- ) REQUIRE-REG:COUNT <> if s\" require-cap: a row was lost\" 3 die then ;" FORGE-APPEND  10 FORGE-C
   s" REQUIRE-REG:COUNT" FORGE-APPEND  10 FORGE-C
   0 begin dup k < while dup FORGE-LINE 1+ repeat drop
   s" SAME-COUNT" FORGE-APPEND  10 FORGE-C
   FORGE$ ;

: STORE ( len len outcome ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N ERR-U !
   oc MATCH outcome
     exited OF   RC-N ! -1 EXITED? ! ENDOF
     signaled OF RC-N !  0 EXITED? ! ENDOF
     timeout OF src u OUT-BUF outu LEN>N ERR-BUF ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

: RUN-CHILD ( n -- )
   FORGE-GEN {: src:ptr u:n :}
   src u
   OUT-BUF IO-CAP >LEN
   ERR-BUF IO-CAP >LEN
   TIMEOUT-MS >MS
   SUBJECT:RUN                          \ -- out-len err-len outcome
   src u STORE ;

: ASSERT-LOADS ( -- )                   \ child accepted every require and exited clean
   EXITED? @ TTRUE
   RC-N @ 0 T= ;

: CASES ( -- )
   s" require inventory grows past 512 entries" T-LABEL
   513 RUN-CHILD ASSERT-LOADS
   s" require inventory grows again" T-LABEL
   650 RUN-CHILD ASSERT-LOADS ;

: MAIN ( -- )
   T-RESET
   CASES
   T-REPORT
   s" require-cap-test: ok" type cr ;

MAIN

;package
