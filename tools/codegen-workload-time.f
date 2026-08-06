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
\ A ROW ALSO SAYS WHAT IT IS FOR. Every row carries a FAMILY - the workload it
\ belongs to - and a KIND. A KIND-REAL row is a comparison someone wants an
\ answer about; a KIND-NULL row ran the same program on both arms, so whatever
\ delta it produced is a delta this harness manufactures out of nothing. The
\ kinds are DESCRIPTIVE: the report prints the null rows beside the real ones
\ so a reader sees the harness's own noise next to the number being read, and
\ nothing here or there turns that comparison into a verdict. The bar-and-
\ verdict machinery this store used to feed (a family's largest null delta as
\ the line a real row had to clear) produced REAL LOSS on unchanged trees -
\ two to four null draws are not a distribution - and was deleted with dot
\ habu-pair-and-alternate-60b04c6a: until the harness is calibrated, the
\ report prints raw data and no verdict stronger than the data.
\
\ A PAIR ROW ALTERNATES ITS ARM ORDER AND KEEPS ITS PAIRED DELTAS. Running the
\ old arm first in every round hands any drift that arrives mid-round to one
\ column systematically; PAIR flips the order each round instead, and beside
\ the two columns' fastest runs it records the per-round paired deltas'
\ extremes - DELTA-LO and DELTA-HI, each round's two runs compared inside the
\ same pair of adjacent windows. That interval is the measured spread of the
\ comparison itself: a reader who sees it straddle zero knows the sign of the
\ headline delta is not supported by the rounds.
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

48 constant ROW-MAX
32 constant NAME-MAX
1000 constant PERMILLE          \ the unit a spread and a delta are reported in

private

$7FFFFFFFFFFFFFFF constant NS-MAX

\ How many publications of one body a placement sweep times against each other.
5 constant SWEEP-ARMS

\ What a row is for. A real row is a comparison to be judged; a null row ran the
\ same program on both arms and is one draw of what this harness invents when
\ nothing changed. There are two openers and no way to write a third value in.
0 constant KIND-REAL
1 constant KIND-NULL

\ ---- a row's two columns, each written as one column --------------------------
\ A row holds three numbers per column: the column's fastest run, its spread, and
\ the value it computed. They live in ONE array per column rather than one array
\ per number, and CLOSE writes each column with a single call, because the thing
\ a reader of the report is trusting is that this column IS the before-arm. Cell
\ by cell, that can be true of the value and false of the time, and two numbers
\ swapped between columns is a delta with its sign inverted and nothing on the
\ page to show it. Written as one column, a column that got the wrong arm got the
\ wrong ANSWER as well - and an answer is a fact a scheduled suite can check,
\ because unlike a time it does not come from a clock.
\
\ THAT ARGUMENT IS ABOUT PAIR ROWS AND NOT ABOUT SWEEP ROWS. A sweep's two
\ columns are the fastest and the slowest of five publications of ONE body, so
\ both computed the same value and the answers cannot tell them apart. What
\ identifies a sweep's columns instead is a fact of its construction rather than
\ of its measurement: the old column is the smallest fastest run and the new one
\ is the largest, so the old is never the greater. That is what the suite states
\ about a sweep row, and it cannot fail for host load - whatever the five times
\ were, the least of them is not the greatest.
\
\ WHAT IS NOT AN ARM COLUMN. A row's name, its family and its kind belong to the
\ ROW and not to either of its arms - a family is what the row is a measurement
\ OF - so they stay row-wide columns and the arm array holds only what an arm
\ measured.
3 constant ARM-CELLS
0 constant ARM-NS
1 constant ARM-SPREAD
2 constant ARM-SUM

ROW-MAX NAME-MAX * BUFFER: NAME-BYTES
create NAME-LENS ROW-MAX cells allot
ROW-MAX NAME-MAX * BUFFER: FAM-BYTES
create FAM-LENS ROW-MAX cells allot
create KIND-A ROW-MAX cells allot
create OLD-ARM ROW-MAX ARM-CELLS * cells allot
create NEW-ARM ROW-MAX ARM-CELLS * cells allot
create REPS-A ROW-MAX cells allot
create ROUNDS-A ROW-MAX cells allot
create WOVEN-A ROW-MAX cells allot
create PAIRED-A ROW-MAX cells allot
create DLO-A ROW-MAX cells allot
create DHI-A ROW-MAX cells allot

create SW-FAST SWEEP-ARMS cells allot
create SW-SLOW SWEEP-ARMS cells allot

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
variable PAIRED-V
variable PR-OLD                 \ one round's old-arm run, for the paired delta
variable PR-NEW                 \ one round's new-arm run, for the paired delta
variable DLO-V                  \ smallest per-round paired delta so far
variable DHI-V                  \ largest per-round paired delta so far

: SLOT ( ptr a n -- ptr a )
   cells + ;

: ROW-OK ( n -- n )
   dup 0 < over ROW-N @ >= or if E-WLTIME-ROW throw then ;

\ Where row k's slice of a string table starts. The base stays on the stack
\ rather than in a local: a `ptr` local would drop the element type the copy and
\ the comparison both need.
: STR-AT ( ptr u8 n -- ptr u8 )
   NAME-MAX * + ;

: NAME-AT ( n -- ptr u8 )
   NAME-BYTES swap STR-AT ;

: FAM-AT ( n -- ptr u8 )
   FAM-BYTES swap STR-AT ;

: ARM-SLOT ( ptr a n n -- ptr a ) {: arm:ptr k:n s:n :}
   arm k ARM-CELLS * s + SLOT ;

: ARM@ ( n ptr a n -- n ) {: k:n arm:ptr s:n :}
   arm k ROW-OK s ARM-SLOT @ ;

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

public

\ How much of an old number a new one saved, in parts per thousand, SIGNED: a new
\ arm that took longer than the old one answers negative. It is a word of its own
\ rather than three tokens inside DELTA-PERMILLE because a scheduled suite can
\ hand it a pair of numbers it chose and check the sign that comes back, while a
\ row's two times only ever arrive from a clock.
: DELTA-OF ( n n -- n ) {: old:n new:n :}
   old 0= if E-WLTIME-CLOCK throw then
   old new - PERMILLE * old / ;

private

: STR-CP ( ptr u8 n ptr u8 -- )
   swap STR-LEN BYTE-COPY-LEN ;

: CAP-CK ( ptr u8 n -- ptr u8 n )
   dup NAME-MAX > if E-WLTIME-CAP throw then ;

: NAME! ( ptr u8 n -- )
   CAP-CK
   dup NAME-LENS ROW-N @ SLOT !
   ROW-N @ NAME-AT STR-CP ;

: FAM! ( ptr u8 n -- )
   CAP-CK
   dup FAM-LENS ROW-N @ SLOT !
   ROW-N @ FAM-AT STR-CP ;

: OPEN-CK ( -- )
   OPEN? @ 0= if E-WLTIME-STATE throw then ;

\ One column's three numbers into one column, in one decision.
: ARM! ( n n n ptr a -- ) {: fast:n slow:n sum:n arm:ptr :}
   fast          arm ROW-N @ ARM-NS ARM-SLOT !
   fast slow SPREAD-OF  arm ROW-N @ ARM-SPREAD ARM-SLOT !
   sum           arm ROW-N @ ARM-SUM ARM-SLOT ! ;

\ ---- opening a row ----------------------------------------------------------
\ A row is opened with its whole identity: what it is called, which workload
\ family it belongs to, and whether it is a comparison to be judged or a null
\ draw that helps judge one. Everything after this only measures.
: OPEN ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n fa:ptr fu:n kind:n :}
   OPEN? @ 0<> if E-WLTIME-STATE throw then
   ROW-N @ ROW-MAX >= if E-WLTIME-CAP throw then
   a u NAME!
   fa fu FAM!
   kind KIND-A ROW-N @ SLOT !
   NS-MAX OLD-FAST !  0 OLD-SLOW !
   NS-MAX NEW-FAST !  0 NEW-SLOW !
   0 HAVE-OLD !  0 HAVE-NEW !  0 HAVE-SUMS !  0 WOVEN !
   0 REPS-V !  0 ROUNDS-V !
   0 PAIRED-V !  NS-MAX DLO-V !  0 NS-MAX - DHI-V !
   -1 OPEN? ! ;

public

: OPEN-REAL ( ptr u8 n ptr u8 n -- )
   KIND-REAL OPEN ;

: OPEN-NULL ( ptr u8 n ptr u8 n -- )
   KIND-NULL OPEN ;

: RESET ( -- )
   0 ROW-N !  0 OPEN? ! ;

: ROWS ( -- n )
   ROW-N @ ;

\ ---- a row measured in two separate phases ----------------------------------
\ The steps a compile-shaped workload needs, because its arms cannot be threaded
\ through each other. A row opened and never closed records nothing, and a row
\ closed with an arm missing is refused rather than reported with half a
\ measurement in it.

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
   OLD-FAST @ OLD-SLOW @ OLD-SUM-V @ OLD-ARM ARM!
   NEW-FAST @ NEW-SLOW @ NEW-SUM-V @ NEW-ARM ARM!
   REPS-V @ REPS-A ROW-N @ SLOT !
   ROUNDS-V @ ROUNDS-A ROW-N @ SLOT !
   WOVEN @ WOVEN-A ROW-N @ SLOT !
   PAIRED-V @ PAIRED-A ROW-N @ SLOT !
   DLO-V @ DLO-A ROW-N @ SLOT !
   DHI-V @ DHI-A ROW-N @ SLOT !
   ROW-N @ 1+ ROW-N !
   0 OPEN? ! ;

\ ---- a row whose two arms are threaded through each other -------------------
\ The ordinary case: both arms are runnable at the same moment, so a round
\ measures one of each and the host's behaviour during the measurement lands on
\ both columns. The row is opened first, by the caller, with the identity it is
\ to be reported under; this word only measures and closes.
\
\ THE ORDER ALTERNATES. A round that always ran the old arm first would hand
\ any drift that arrives mid-round - a thermal step, a scheduler migration -
\ to the same column every time, which is a bias no number on the page shows.
\ Even rounds run old-then-new and odd rounds new-then-old, so whatever a
\ round's two windows differ by lands on each column equally often.
\
\ AND EACH ROUND'S PAIR IS A DATUM OF ITS OWN. The two runs of one round sit
\ in adjacent windows, so their delta is the comparison made under one host
\ moment; PR-NOTE folds each round's delta into the row's DELTA-LO..DELTA-HI
\ interval, which the report prints beside the fastest-run delta. An interval
\ that straddles zero says the rounds did not agree on a direction.
private
: PR-NOTE ( -- )
   PR-OLD @ PR-NEW @ DELTA-OF {: d:n :}
   d DLO-V @ < if d DLO-V ! then
   d DHI-V @ > if d DHI-V ! then ;

\ typed-local-lint: allow-bare-local - old and new are the two arms' bodies, and
\ a local annotation cannot carry a quotation effect.
: PAIR-ROUND ( n n [ -- ] [ -- ] -- ) {: r:n reps:n old new :}
   r 1 and 0= if
      reps old RUN-ONCE dup SAMPLE-OLD PR-OLD !
      reps new RUN-ONCE dup SAMPLE-NEW PR-NEW !
   else
      reps new RUN-ONCE dup SAMPLE-NEW PR-NEW !
      reps old RUN-ONCE dup SAMPLE-OLD PR-OLD !
   then
   PR-NOTE ;
public

\ typed-local-lint: allow-bare-local - old and new are the two arms' bodies, and
\ a local annotation cannot carry a quotation effect.
: PAIR ( n n n n [ -- ] [ -- ] -- )
   {: reps:n rounds:n oldsum:n newsum:n old new :}
   OPEN-CK
   rounds 0 ?do
      i reps old new PAIR-ROUND
   loop
   oldsum newsum ANSWERS
   reps REPS-V !  rounds ROUNDS-V !
   -1 HAVE-OLD !  -1 HAVE-NEW !  -1 WOVEN !  -1 PAIRED-V !
   CLOSE ;

\ ---- a row that times one body at several addresses -------------------------
\ A PLACEMENT SWEEP. Its arms are SWEEP-ARMS drivers over identical code that
\ reach identical copies of one subject, published one after another and
\ differing in nothing but the addresses they landed at. A round runs every arm
\ once, so the rounds thread all of them through each other exactly as PAIR
\ threads two, and each arm's fastest run comes out of the same sequence of
\ windows as every other arm's.
\
\ WHAT IT RECORDS, AND WHY IT FITS A TWO-ARM ROW. The row keeps the FASTEST of
\ the publications in the old column and the SLOWEST in the new one, with each of
\ those two arms' own spread. The row's delta is then the widest gap between any
\ two of the publications - which is precisely the largest delta this harness can
\ report for a body nobody changed, and is the bar its family's real row has to
\ clear. Recording the two extremes rather than every arm is not a summary that
\ loses the question: the question IS the widest gap, and a pair drawn from
\ anywhere inside the sweep is smaller than it by definition.
\
\ WHY THE EXTREMES AND NOT A CHOSEN PAIR. An earlier form of this measured a
\ fixed reference publication against each of the others. That misses the widest
\ gap whenever the reference sits in the middle of the spread, and the effect is
\ not a wobble around a centre: on the scan shape the publications fall into a
\ fast group and a slow group forty per cent apart, so which pair you happened to
\ name decided the bar.
private

: SW-INIT ( -- )
   SWEEP-ARMS 0 ?do
      NS-MAX SW-FAST i SLOT !
      0 SW-SLOW i SLOT !
   loop ;

\ typed-local-lint: allow-bare-local - q is the arm's body, as in ARM-OLD.
: SW-RUN ( n n [ -- ] -- ) {: arm:n reps:n q :}
   reps q RUN-ONCE {: ns:n :}
   ns SW-FAST arm SLOT @ < if ns SW-FAST arm SLOT ! then
   ns SW-SLOW arm SLOT @ > if ns SW-SLOW arm SLOT ! then ;

\ The arm with the smallest fastest run, and the arm with the largest.
: SW-BEST ( -- n )
   0
   SWEEP-ARMS 0 ?do
      SW-FAST i SLOT @  over SW-FAST swap SLOT @  < if drop i then
   loop ;

: SW-WORST ( -- n )
   0
   SWEEP-ARMS 0 ?do
      SW-FAST i SLOT @  over SW-FAST swap SLOT @  > if drop i then
   loop ;

: SW-EXTREMES ( -- )
   SW-BEST {: b:n :}
   SW-WORST {: w:n :}
   SW-FAST b SLOT @ OLD-FAST !  SW-SLOW b SLOT @ OLD-SLOW !
   SW-FAST w SLOT @ NEW-FAST !  SW-SLOW w SLOT @ NEW-SLOW ! ;

public

\ typed-local-lint: allow-bare-local - q1..q5 are the arms' bodies, and a local
\ annotation cannot carry a quotation effect.
: SWEEP ( n n n [ -- ] [ -- ] [ -- ] [ -- ] [ -- ] -- )
   {: reps:n rounds:n sum:n q1 q2 q3 q4 q5 :}
   OPEN-CK
   SW-INIT
   rounds 0 ?do
      0 reps q1 SW-RUN
      1 reps q2 SW-RUN
      2 reps q3 SW-RUN
      3 reps q4 SW-RUN
      4 reps q5 SW-RUN
   loop
   SW-EXTREMES
   sum sum ANSWERS
   reps REPS-V !  rounds ROUNDS-V !
   -1 HAVE-OLD !  -1 HAVE-NEW !  -1 WOVEN !
   CLOSE ;

\ ---- reading a row back -----------------------------------------------------

: NAME$ ( n -- ptr u8 n ) {: k:n :}
   k ROW-OK NAME-AT
   NAME-LENS k SLOT @ ;

: FAM$ ( n -- ptr u8 n ) {: k:n :}
   k ROW-OK FAM-AT
   FAM-LENS k SLOT @ ;

private

: KIND ( n -- n ) {: k:n :}
   KIND-A k ROW-OK SLOT @ ;

public

: NULL? ( n -- bool ) {: k:n :}
   k KIND KIND-NULL = ;

: REAL? ( n -- bool ) {: k:n :}
   k KIND KIND-REAL = ;

: OLD-NS ( n -- n )
   OLD-ARM ARM-NS ARM@ ;

: NEW-NS ( n -- n )
   NEW-ARM ARM-NS ARM@ ;

: OLD-SPREAD ( n -- n )
   OLD-ARM ARM-SPREAD ARM@ ;

: NEW-SPREAD ( n -- n )
   NEW-ARM ARM-SPREAD ARM@ ;

: OLD-SUM ( n -- n )
   OLD-ARM ARM-SUM ARM@ ;

: NEW-SUM ( n -- n )
   NEW-ARM ARM-SUM ARM@ ;

: REPS ( n -- n ) {: k:n :}
   REPS-A k ROW-OK SLOT @ ;

: ROUNDS ( n -- n ) {: k:n :}
   ROUNDS-A k ROW-OK SLOT @ ;

: INTERLEAVED? ( n -- bool ) {: k:n :}
   WOVEN-A k ROW-OK SLOT @ 0<> ;

\ Whether the row's rounds were measured as alternating pairs, and the extremes
\ of its per-round paired deltas. Only a PAIR row has them: a split row's arms
\ never shared a window and a sweep's columns are two of five publications, so
\ asking either for a paired interval is a caller error, not a zero.
: PAIRED? ( n -- bool ) {: k:n :}
   PAIRED-A k ROW-OK SLOT @ 0<> ;

: PAIRED-CK ( n -- n ) {: k:n :}
   k PAIRED? 0= if E-WLTIME-STATE throw then
   k ;

: DELTA-LO ( n -- n ) {: k:n :}
   DLO-A k PAIRED-CK SLOT @ ;

: DELTA-HI ( n -- n ) {: k:n :}
   DHI-A k PAIRED-CK SLOT @ ;

\ Did the two arms compute the same thing? The delta means nothing unless they
\ did.
: SAME-ANSWER? ( n -- bool ) {: k:n :}
   k OLD-SUM k NEW-SUM = ;

\ How much of the old arm's time the new arm saved, in parts per thousand. A
\ negative row is one the new code generator lost.
: DELTA-PERMILLE ( n -- n ) {: k:n :}
   k OLD-NS  k NEW-NS  DELTA-OF ;

: ROW-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup ROW-N @ < while
      dup NAME$ a u STR= if exit then
      1+
   repeat drop -1 ;

;package
