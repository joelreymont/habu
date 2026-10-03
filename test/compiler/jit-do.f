\ jit-do.f - counted-loop entry in the default JIT tier.
\
\ `do` always takes its first turn. `?do ... loop` enters only while start <
\ limit, signed, so a count at or below zero takes no turn at all. `?do ...
\ +loop` skips only equal bounds: its step is a per-turn value, and a loop
\ counting down has its limit below its start. Every loop records the indices it
\ visits, so one that took the right number of turns from the wrong index fails
\ too, and every loop leaves at an explicit turn cap, so a broken entry test
\ fails a row instead of running round the integer range.
\ test/compiler/native-do.f and native-plusloop.f hold the tier-1 twins.
require lib/test.f
require lib/prelude.f

package JDO-FIXTURE
public

16 constant VISITED-CAP
512 constant TURN-CAP
create VISITED VISITED-CAP cells allot
variable ITERATIONS

: VISIT ( n -- )
   ITERATIONS @ VISITED-CAP < if ITERATIONS @ cells VISITED + ! else drop then
   1 ITERATIONS +! ;

: VISITED@ ( n -- n )
   cells VISITED + @ ;

: CAPPED? ( -- bool )
   ITERATIONS @ TURN-CAP >= ;

: DO-TURNS ( n n -- n ) {: lim:n st:n :}
   0 ITERATIONS !
   lim st do i VISIT CAPPED? if leave then loop
   ITERATIONS @ ;

: QDO-TURNS ( n n -- n ) {: lim:n st:n :}
   0 ITERATIONS !
   lim st ?do i VISIT CAPPED? if leave then loop
   ITERATIONS @ ;

: QDO-STEP ( n n n -- n ) {: lim:n st:n step:n :}
   0 ITERATIONS !
   lim st ?do i VISIT CAPPED? if leave then step +loop
   ITERATIONS @ ;

\ Nested openers closed different ways: each `+loop` settles the entry test of
\ the `?do` it closes and no other.
: NEST-DOWN ( -- n )
   0 ITERATIONS !
   2 0 ?do 0 3 ?do j 10 * i + VISIT -1 +loop loop
   ITERATIONS @ ;

: NEST-UP ( -- n )
   0 ITERATIONS !
   0 3 ?do 2 0 ?do j 10 * i + VISIT loop -1 +loop
   ITERATIONS @ ;

: NEST-LEAVE ( -- n )
   0 ITERATIONS !
   2 0 ?do 0 3 ?do i 1 = if leave then j 10 * i + VISIT -1 +loop loop
   ITERATIONS @ ;

: NEST-EXIT ( -- )
   0 ITERATIONS !
   2 0 ?do 0 3 ?do
      j 10 * i + 11 = if unloop unloop exit then
      j 10 * i + VISIT
   -1 +loop loop ;

\ A `do` opened at the level an earlier `?do` used: the `+loop` closing the `do`
\ has no entry test to settle and must leave the earlier loop's alone.
: STALE ( -- n )
   0 ITERATIONS !
   -1 0 ?do i VISIT loop
   0 3 do i VISIT -1 +loop
   ITERATIONS @ ;

;package

package JDO-TEST
private

$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT
$100000000000000 constant BIG-STEP     \ 256 of these go once round the integers

: VISITED-IS ( n n -- ) {: idx:n want:n :}
   idx JDO-FIXTURE:VISITED@ want T= ;

: BELOW-CASE ( -- )
   s" ?do loop takes no turn with the limit below the start" T-LABEL
   -1 0 JDO-FIXTURE:QDO-TURNS 0 T=
   MIN-INT 0 JDO-FIXTURE:QDO-TURNS 0 T=
   -3 0 JDO-FIXTURE:QDO-TURNS 0 T=
   0 1 JDO-FIXTURE:QDO-TURNS 0 T=
   MIN-INT MAX-INT JDO-FIXTURE:QDO-TURNS 0 T=
   s" ?do loop takes no turn at equal bounds" T-LABEL
   0 0 JDO-FIXTURE:QDO-TURNS 0 T=
   5 5 JDO-FIXTURE:QDO-TURNS 0 T=
   -1 -1 JDO-FIXTURE:QDO-TURNS 0 T= ;

: ABOVE-CASE ( -- )
   s" ?do loop visits every index from the start up to the limit" T-LABEL
   3 0 JDO-FIXTURE:QDO-TURNS 3 T=
   0 0 VISITED-IS  1 1 VISITED-IS  2 2 VISITED-IS
   0 -2 JDO-FIXTURE:QDO-TURNS 2 T=
   0 -2 VISITED-IS  1 -1 VISITED-IS
   5 2 JDO-FIXTURE:QDO-TURNS 3 T=
   0 2 VISITED-IS  2 4 VISITED-IS
   -3 -5 JDO-FIXTURE:QDO-TURNS 2 T=
   0 -5 VISITED-IS  1 -4 VISITED-IS
   s" ?do loop counts at both ends of the signed range" T-LABEL
   MAX-INT MAX-INT 2 - JDO-FIXTURE:QDO-TURNS 2 T=
   1 MAX-INT 1- VISITED-IS
   MIN-INT 2 + MIN-INT JDO-FIXTURE:QDO-TURNS 2 T=
   0 MIN-INT VISITED-IS ;

: DO-CASE ( -- )
   s" do takes its first turn whatever the bounds" T-LABEL
   -1 0 JDO-FIXTURE:DO-TURNS 1 T=
   0 0 VISITED-IS
   MIN-INT 0 JDO-FIXTURE:DO-TURNS 1 T=
   0 0 JDO-FIXTURE:DO-TURNS 1 T=
   3 0 JDO-FIXTURE:DO-TURNS 3 T= ;

: STEP-CASE ( -- )
   s" ?do +loop counts down to a limit below its start" T-LABEL
   0 10 -1 JDO-FIXTURE:QDO-STEP 11 T=
   0 10 VISITED-IS  10 0 VISITED-IS
   0 3 -1 JDO-FIXTURE:QDO-STEP 4 T=
   0 3 VISITED-IS  1 2 VISITED-IS  2 1 VISITED-IS  3 0 VISITED-IS
   0 10 -3 JDO-FIXTURE:QDO-STEP 4 T=
   3 1 VISITED-IS
   s" ?do +loop skips only equal bounds" T-LABEL
   0 0 1 JDO-FIXTURE:QDO-STEP 0 T=
   7 7 -1 JDO-FIXTURE:QDO-STEP 0 T=
   10 0 4 JDO-FIXTURE:QDO-STEP 3 T=
   s" a rising step from above the limit goes once round the integers" T-LABEL
   -1 0 BIG-STEP JDO-FIXTURE:QDO-STEP 256 T= ;

: NEST-CASE ( -- )
   s" a +loop inside a ?do loop settles only its own entry test" T-LABEL
   JDO-FIXTURE:NEST-DOWN 8 T=
   0 3 VISITED-IS  3 0 VISITED-IS  4 13 VISITED-IS  7 10 VISITED-IS
   s" a ?do loop inside a +loop keeps its own entry test" T-LABEL
   JDO-FIXTURE:NEST-UP 8 T=
   0 30 VISITED-IS  1 31 VISITED-IS  6 0 VISITED-IS  7 1 VISITED-IS
   s" leave and unloop exit out of a counting-down ?do" T-LABEL
   JDO-FIXTURE:NEST-LEAVE 4 T=
   1 2 VISITED-IS  2 13 VISITED-IS
   JDO-FIXTURE:NEST-EXIT  JDO-FIXTURE:ITERATIONS @ 6 T=
   5 12 VISITED-IS
   s" a do at an earlier ?do's level leaves that entry test alone" T-LABEL
   JDO-FIXTURE:STALE 4 T=
   0 3 VISITED-IS  3 0 VISITED-IS ;

public

: RUN ( -- )
   BELOW-CASE
   ABOVE-CASE
   DO-CASE
   STEP-CASE
   NEST-CASE ;

;package

T-RESET
JDO-TEST:RUN
T-REPORT
