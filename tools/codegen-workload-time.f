\ codegen-workload-time.f - time two arms of one workload against each other and
\ keep the row. One concern: turning two runnable arms into one recorded delta.
\
\ THE DISCIPLINE IS tools/codegen-compare-core.f's, WITH ONE ADDITION. That file
\ established it and states the reasoning in full: one timed run executes the
\ body a fixed number of times, the elapsed monotonic nanoseconds are the
\ measurement, and each run is repeated. A run can only be made slower by the
\ rest of the machine, never faster, so the FASTEST of several runs is the
\ closest estimate of the real cost available, and the spread between fastest and
\ slowest says how noisy the host was while the row was measured.
\
\ THE ADDITION IS INTERLEAVING, and it is the whole reason this file exists
\ rather than a second call into that one. Here the two numbers being compared
\ are produced in ONE process, and what separates them is a migration that
\ publishes new code, grows the code region and moves every later definition. If
\ the old arm's runs all happened before the new arm's, a thermal ramp or a
\ scheduler decision that arrived in between would land entirely on one column
\ and be indistinguishable from a code change. So a round times the old arm and
\ then the new arm, and the rounds repeat: whatever the host does during the
\ measurement is spread across both columns, and the fastest-run rule then picks
\ each column's cleanest window out of the same sequence of windows.
\
\ ONE KIND OF WORKLOAD CANNOT BE INTERLEAVED, AND SAYS SO IN ITS ROW. A workload
\ that COMPILES something can only be run before the migration or after it - the
\ migration is the event that separates the two arms, and a compile-shaped arm
\ consumes dictionary space it cannot give back, so its rounds cannot be
\ threaded through the migration. Such a row is opened, given its old arm, and
\ completed with its new arm later; INTERLEAVED? answers false for it and the
\ report prints that beside the delta, because a reader judging a two per cent
\ difference needs to know which of the two kinds of row it is.
\
\ WHAT A ROW HOLDS, AND WHY THE ANSWERS ARE IN IT. Each arm records its fastest
\ run, its spread, and the VALUE its workload computed. The last one is not
\ decoration: a workload's two arms run different machine code over the same
\ data, and the only evidence that they ran the same PROGRAM is that they reached
\ the same answer. A row whose two answers disagree is a miscompilation, and the
\ report names it as one rather than timing it and folding it into a claim.
\
\ THE DELTA IS REPORTED, NEVER ASSERTED HERE. Nothing in this file compares a
\ measurement with a committed number or throws because a row was slower than
\ expected: a timing that can fail is a timing that fails for host load, and the
\ standing rule is that the scheduled suites hold facts and the hand-run report
\ holds timings. What IS checked is that the clock moved at all, because a zero
\ elapsed run would silently divide the whole report by nothing.

require lib/errors.f
require lib/prelude.f
require lib/string.f

package CODEGEN-CLOCK

public

-7221 constant E-WLTIME-CAP     \ the row store is full, or a name longer than a row holds
-7222 constant E-WLTIME-ROW     \ a row index outside the recorded count
-7223 constant E-WLTIME-CLOCK   \ the monotonic clock reported no elapsed time across a whole run
-7224 constant E-WLTIME-STATE   \ an arm measured with no row open, or a row closed with an arm missing

16 constant ROW-MAX
32 constant NAME-MAX
1000 constant PERMILLE          \ the unit a spread and a delta are reported in

private

$7FFFFFFFFFFFFFFF constant NS-MAX

ROW-MAX NAME-MAX * BUFFER: NAME-BYTES
create NAME-LENS ROW-MAX cells allot
create OLD-NS-A ROW-MAX cells allot
create NEW-NS-A ROW-MAX cells allot
create OLD-SPREAD-A ROW-MAX cells allot
create NEW-SPREAD-A ROW-MAX cells allot
create OLD-SUM-A ROW-MAX cells allot
create NEW-SUM-A ROW-MAX cells allot
create REPS-A ROW-MAX cells allot
create ROUNDS-A ROW-MAX cells allot
create WOVEN-A ROW-MAX cells allot

variable ROW-N
variable OPEN?                  \ a row is being measured
variable HAVE-OLD
variable HAVE-NEW
variable HAVE-SUMS
variable WOVEN
variable OLD-FAST
variable OLD-SLOW
variable NEW-FAST
variable NEW-SLOW
variable OLD-SUM-V
variable NEW-SUM-V
variable REPS-V
variable ROUNDS-V

: SLOT ( ptr a n -- ptr a )
   cells + ;

: ROW-OK ( n -- n )
   dup 0 < over ROW-N @ >= or if E-WLTIME-ROW throw then ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAME-BYTES + ;

\ ---- one timed run ----------------------------------------------------------
\ typed-local-lint: allow-bare-local - q is the timing body; its effect is in the
\ stack signature and a local annotation cannot carry a quotation effect.
: RUN-ONCE ( n [ -- ] -- n ) {: reps:n q :}
   mono-ns {: t0:n :}
   reps 0 ?do q execute loop
   mono-ns t0 - ;

: SAMPLE-OLD ( n -- ) {: ns:n :}
   ns OLD-FAST @ < if ns OLD-FAST ! then
   ns OLD-SLOW @ > if ns OLD-SLOW ! then ;

: SAMPLE-NEW ( n -- ) {: ns:n :}
   ns NEW-FAST @ < if ns NEW-FAST ! then
   ns NEW-SLOW @ > if ns NEW-SLOW ! then ;

: SPREAD-OF ( n n -- n ) {: fast:n slow:n :}
   fast 0= if E-WLTIME-CLOCK throw then
   slow fast - PERMILLE * fast / ;

: NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u NAME-MAX > if E-WLTIME-CAP throw then
   a  ROW-N @ NAME-AT  u STR-LEN BYTE-COPY-LEN
   u NAME-LENS ROW-N @ SLOT ! ;

: OPEN-CK ( -- )
   OPEN? @ 0= if E-WLTIME-STATE throw then ;

public

: RESET ( -- )
   0 ROW-N !  0 OPEN? ! ;

: ROWS ( -- n )
   ROW-N @ ;

\ ---- a row measured in two separate phases ----------------------------------
\ The three steps a compile-shaped workload needs, because its arms cannot be
\ threaded through each other. A row opened and never closed records nothing,
\ and a row closed with an arm missing is refused rather than reported with half
\ a measurement in it.
: OPEN ( ptr u8 n -- ) {: a:ptr u:n :}
   OPEN? @ 0<> if E-WLTIME-STATE throw then
   ROW-N @ ROW-MAX >= if E-WLTIME-CAP throw then
   a u NAME!
   NS-MAX OLD-FAST !  0 OLD-SLOW !
   NS-MAX NEW-FAST !  0 NEW-SLOW !
   0 HAVE-OLD !  0 HAVE-NEW !  0 HAVE-SUMS !  0 WOVEN !
   0 REPS-V !  0 ROUNDS-V !
   -1 OPEN? ! ;

\ typed-local-lint: allow-bare-local - q is the arm's body, and a local
\ annotation cannot carry a quotation effect.
: ARM-OLD ( n n [ -- ] -- ) {: reps:n rounds:n q :}
   OPEN-CK
   rounds 0 ?do reps q RUN-ONCE SAMPLE-OLD loop
   reps REPS-V !  rounds ROUNDS-V !
   -1 HAVE-OLD ! ;

\ typed-local-lint: allow-bare-local - q is the arm's body, as in ARM-OLD.
: ARM-NEW ( n n [ -- ] -- ) {: reps:n rounds:n q :}
   OPEN-CK
   rounds 0 ?do reps q RUN-ONCE SAMPLE-NEW loop
   reps REPS-V !  rounds ROUNDS-V !
   -1 HAVE-NEW ! ;

\ What the two arms computed. A split row's answers are only knowable after the
\ arm has run - a compile-shaped arm's answer is how much it compiled - so they
\ are handed over here rather than to the arm, and CLOSE refuses a row that never
\ got them.
: ANSWERS ( n n -- ) {: oldsum:n newsum:n :}
   OPEN-CK
   oldsum OLD-SUM-V !  newsum NEW-SUM-V !
   -1 HAVE-SUMS ! ;

: CLOSE ( -- )
   OPEN-CK
   HAVE-OLD @ 0= HAVE-NEW @ 0= or if E-WLTIME-STATE throw then
   HAVE-SUMS @ 0= if E-WLTIME-STATE throw then
   OLD-FAST @ 0= NEW-FAST @ 0= or if E-WLTIME-CLOCK throw then
   OLD-FAST @ OLD-NS-A ROW-N @ SLOT !
   NEW-FAST @ NEW-NS-A ROW-N @ SLOT !
   OLD-FAST @ OLD-SLOW @ SPREAD-OF OLD-SPREAD-A ROW-N @ SLOT !
   NEW-FAST @ NEW-SLOW @ SPREAD-OF NEW-SPREAD-A ROW-N @ SLOT !
   OLD-SUM-V @ OLD-SUM-A ROW-N @ SLOT !
   NEW-SUM-V @ NEW-SUM-A ROW-N @ SLOT !
   REPS-V @ REPS-A ROW-N @ SLOT !
   ROUNDS-V @ ROUNDS-A ROW-N @ SLOT !
   WOVEN @ WOVEN-A ROW-N @ SLOT !
   ROW-N @ 1+ ROW-N !
   0 OPEN? ! ;

\ ---- a row whose two arms are threaded through each other -------------------
\ The ordinary case: both arms are runnable at the same moment, so a round
\ measures one of each and the host's behaviour during the measurement lands on
\ both columns.
\ typed-local-lint: allow-bare-local - old and new are the two arms' bodies, and
\ a local annotation cannot carry a quotation effect.
: PAIR ( ptr u8 n n n n n [ -- ] [ -- ] -- )
   {: a:ptr u:n reps:n rounds:n oldsum:n newsum:n old new :}
   a u OPEN
   rounds 0 ?do
      reps old RUN-ONCE SAMPLE-OLD
      reps new RUN-ONCE SAMPLE-NEW
   loop
   oldsum newsum ANSWERS
   reps REPS-V !  rounds ROUNDS-V !
   -1 HAVE-OLD !  -1 HAVE-NEW !  -1 WOVEN !
   CLOSE ;

\ ---- reading a row back -----------------------------------------------------

: NAME$ ( n -- ptr u8 n ) {: k:n :}
   k ROW-OK NAME-AT
   NAME-LENS k SLOT @ ;

: OLD-NS ( n -- n ) {: k:n :}
   OLD-NS-A k ROW-OK SLOT @ ;

: NEW-NS ( n -- n ) {: k:n :}
   NEW-NS-A k ROW-OK SLOT @ ;

: OLD-SPREAD ( n -- n ) {: k:n :}
   OLD-SPREAD-A k ROW-OK SLOT @ ;

: NEW-SPREAD ( n -- n ) {: k:n :}
   NEW-SPREAD-A k ROW-OK SLOT @ ;

: OLD-SUM ( n -- n ) {: k:n :}
   OLD-SUM-A k ROW-OK SLOT @ ;

: NEW-SUM ( n -- n ) {: k:n :}
   NEW-SUM-A k ROW-OK SLOT @ ;

: REPS ( n -- n ) {: k:n :}
   REPS-A k ROW-OK SLOT @ ;

: ROUNDS ( n -- n ) {: k:n :}
   ROUNDS-A k ROW-OK SLOT @ ;

: INTERLEAVED? ( n -- bool ) {: k:n :}
   WOVEN-A k ROW-OK SLOT @ 0<> ;

\ Did the two arms compute the same thing? The delta means nothing unless they
\ did.
: SAME-ANSWER? ( n -- bool ) {: k:n :}
   k OLD-SUM k NEW-SUM = ;

\ How much of the old arm's time the new arm saved, in parts per thousand. A
\ negative row is one the new code generator lost.
: DELTA-PERMILLE ( n -- n ) {: k:n :}
   k OLD-NS {: o:n :}
   o 0= if E-WLTIME-CLOCK throw then
   o k NEW-NS - PERMILLE * o / ;

\ The larger of the two arms' spreads: the noise floor a reader has to hold this
\ row's delta against.
: NOISE-PERMILLE ( n -- n ) {: k:n :}
   k OLD-SPREAD {: o:n :}
   k NEW-SPREAD {: v:n :}
   o v > if o exit then
   v ;

\ Is this row's delta larger than the noise the run itself measured? A reader's
\ aid, printed beside every row; nothing in any scheduled suite turns on it.
: OVER-NOISE? ( n -- bool ) {: k:n :}
   k DELTA-PERMILLE {: d:n :}
   d 0 < if 0 d - else d then
   k NOISE-PERMILLE > ;

: ROW-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup ROW-N @ < while
      dup NAME$ a u STR= if exit then
      1+
   repeat drop -1 ;

;package
