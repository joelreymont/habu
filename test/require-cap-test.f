\ require-cap-test.f - REQUIRE-MAX inventory-cap regression.
\
\ REQUIRE-MAX (src/core/include.f) caps the per-image require inventory; the die
\ site REQUIRE-CHECK-ROOM fails closed ("require: too many files", INCLUDE-IO-RC)
\ once REQUIRE-N reaches the cap. The standalone full-image maki/test.f inventory
\ reached the old $100 (256) cap exactly (dot habu-raise-require-max), so the cap
\ was raised to $200 (512); this pins the fail-closed named die at the new
\ boundary and proves the raise.
\
\ Each case forks a disposable SUBJECT child that resets the require registry,
\ flips DISCOVERY so synthetic paths never touch the filesystem (required stores
\ then skips the load), and drives `require` to a target count. The dedup scan and
\ die site both run on the real loader path.
\
\ Red-first (2026-07-20, base seed cap 256): "one past the old cap loads" and "a
\ full new-cap inventory loads" die at the 257th require (rc 74) and go RED under
\ the unraised engine; the named-overflow case stays GREEN either way. Under the
\ raised $200 cap all three are GREEN.
\
\ Fork-included by test/gate-stdlib-inline-lib.f (stdlib/tail-process), alongside
\ the sister capacity regression test/seal.f.

require lib/test.f
require lib/string.f
require lib/process.f            \ outcome sumtype for the child completion
require lib/test/subject.f       \ SUBJECT:RUN - isolated evaluation of the subject

package REQUIRE-CAP

256 constant OLD-CAP             \ the $100 cap this dot raised on 2026-07-20
$4000 constant FORGE-CAP         \ REQUIRE-MAX+1 "require pXX" lines fit well under this
$800 constant IO-CAP
30000 constant TIMEOUT-MS

create FORGE-BUF FORGE-CAP allot
variable FORGE-U
create OUT-BUF IO-CAP allot
create ERR-BUF IO-CAP allot
variable ERR-U
variable RC-N
variable EXITED?

\ ---- forge builder: a child program that fills the require table to K entries --

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

: FORGE-GEN ( n -- ptr u8 n ) {: k:n :}
   0 FORGE-U !
   s" 0 REQUIRE-N ! 0 REQUIRE-BASE ! DISCOVERY-ON" FORGE-APPEND  10 FORGE-C
   0 begin dup k < while dup FORGE-LINE 1+ repeat drop
   FORGE$ ;

\ ---- fork a child on the forge, capture its completion + stderr ---------------

: STORE ( outcome -- )
   MATCH outcome
     exited OF   RC-N ! -1 EXITED? ! ENDOF
     signaled OF RC-N !  0 EXITED? ! ENDOF
     timeout OF  0 RC-N !  0 EXITED? ! ENDOF
   ;MATCH ;

: RUN-CHILD ( n -- )
   FORGE-GEN
   OUT-BUF IO-CAP >LEN
   ERR-BUF IO-CAP >LEN
   TIMEOUT-MS >MS
   SUBJECT:RUN                          \ -- out-len err-len outcome
   STORE
   LEN>N ERR-U !
   LEN>N drop ;

: ERR$ ( -- ptr u8 n )  ERR-BUF ERR-U @ ;

: ASSERT-LOADS ( -- )                   \ child accepted every require and exited clean
   EXITED? @ TTRUE
   RC-N @ 0 T= ;

: ASSERT-CAP-DIE ( -- )                 \ child failed closed with the named cap die
   EXITED? @ TTRUE
   RC-N @ INCLUDE-IO-RC T=
   ERR$ s" require: too many files" CONTAINS? TTRUE ;

\ ---- cases -------------------------------------------------------------------

: CASES ( -- )
   s" one require past the old 256 cap now loads" T-LABEL
   OLD-CAP 1+ RUN-CHILD ASSERT-LOADS
   s" a full new-cap (REQUIRE-MAX) inventory loads" T-LABEL
   REQUIRE-MAX RUN-CHILD ASSERT-LOADS
   s" one require past the new cap still dies named" T-LABEL
   REQUIRE-MAX 1+ RUN-CHILD ASSERT-CAP-DIE ;

: MAIN ( -- )
   T-RESET
   CASES
   T-REPORT
   s" require-cap-test: ok" type cr ;

MAIN

;package
