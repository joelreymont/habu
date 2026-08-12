\ judge/row.f - the judged table: one row per corpus subject, three columns, and
\ the verdict each row comes to. One concern: what a run measured and what it
\ decides.
\
\ WHAT A ROW HOLDS. The subject's name, and for each of the three code
\ generators - the engine's emitter, the native chain, and clang on a C twin -
\ how many bytes of machine code it emitted for that subject. The chain's column
\ holds one thing the other two cannot: a REFUSAL CODE, when the chain declined
\ to compile the subject at all.
\
\ WHY A REFUSAL IS A COLUMN VALUE AND NOT AN EXEMPTION. The comparison this
\ replaces kept a hand-written list of the subjects the chain cannot compile, so
\ that a shorter column read as named missing capabilities rather than as a
\ harness that stopped looking. The list is a statement somebody typed: it
\ cannot notice a row that started being refused, and a row that STOPPED being
\ refused stays on it until a person takes it off. Here the refusal is what the
\ compiler answered this run, recorded with its own code, and it is a verdict of
\ its own. A row that starts refusing turns from `smaller` into `REFUSED` on the
\ next run and the committed artifact disagrees; a row that stops refusing turns
\ the other way and the artifact disagrees again. Neither needs anybody to
\ remember anything.
\
\ THE FIVE VERDICTS, AND WHICH OF THEM ARE FINDINGS.
\
\   smaller   the chain emitted fewer bytes than the engine's emitter
\   equal     the same number of bytes
\   LARGER    more bytes: a finding, named and counted, and the run exits
\             non-zero on it
\   REFUSED   the chain declined the subject, with the code it declined it with:
\             a raw measurement, printed with its code, and not a finding - the
\             capability it waits for is a dot, and a run that failed on it would
\             fail every day until that dot lands
\   uncovered no clang twin for this subject, which is a fact about the C file
\             and not about either code generator
\
\ WHY THIS TABLE IS BYTES AND NOT COSTS. A byte count is read off the word's own
\ dictionary record: the same number on every host, in every run, moving only
\ when a compiler moves it, so it can be compared exactly against a committed
\ artifact. A cost is a measurement, and a machine with every core busy - which
\ is what a gate is - moves one by more than any honest tolerance would catch,
\ so a cost column belongs beside this one and not inside it. There is none here
\ yet: dot habu-judge-both-chains-2b07fd19 carries bringing the three columns'
\ costs over from bin/hb --load tools/codegen-compare.f, which measures them
\ today and is run by hand on a quiet machine.

require lib/errors.f
require lib/prelude.f
require lib/string.f

package JUDGE-ROW

private

64 constant CAP-MAX               \ subjects one judged table holds
48 constant NAME-MAX

CAP-MAX NAME-MAX * BUFFER: NAMES
create NAME-LENS CAP-MAX cells allot
create OLD-BYTES CAP-MAX cells allot
create NEW-BYTES CAP-MAX cells allot
create NEW-RC CAP-MAX cells allot          \ 0, or the code the chain refused with
create REF-BYTES CAP-MAX cells allot       \ -1 when the subject has no C twin

variable FILL

: SLOT ( ptr a n -- ptr a )
   cells + ;

: OK ( n -- n )
   dup 0 < over FILL @ >= or if E-JUDGE-ROW-INDEX throw then ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAMES + ;

public

-1 constant NO-REFERENCE           \ this subject has no C twin

: RESET ( -- )
   0 FILL ! ;

: ROWS ( -- n )
   FILL @ ;

\ Open a row for a subject. Everything else about it is filled in by the column
\ that measured it, so a column that never ran leaves its own cells alone rather
\ than writing a zero that would read as a measurement.
: OPEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   FILL @ CAP-MAX >= if E-JUDGE-ROW-CAP throw then
   u NAME-MAX > if E-JUDGE-ROW-CAP throw then
   a  FILL @ NAME-AT  u STR-LEN BYTE-COPY-LEN
   u NAME-LENS FILL @ SLOT !
   0 OLD-BYTES FILL @ SLOT !
   0 NEW-BYTES FILL @ SLOT !
   0 NEW-RC FILL @ SLOT !
   NO-REFERENCE REF-BYTES FILL @ SLOT !
   FILL @
   FILL @ 1+ FILL ! ;

: NAME$ ( n -- ptr u8 n ) {: k:n :}
   k OK NAME-AT
   NAME-LENS k SLOT @ ;

: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   FILL @ 0 ?do
      i NAME$ a u STR= if drop i then
   loop ;

\ ---- what each column writes -------------------------------------------------

: OLD! ( n n -- ) {: k:n bytes:n :}
   k OK drop
   bytes OLD-BYTES k SLOT ! ;

\ The chain compiled it, into this many bytes.
: NEW! ( n n -- ) {: k:n bytes:n :}
   k OK drop
   0 NEW-RC k SLOT !
   bytes NEW-BYTES k SLOT ! ;

\ The chain refused it, with this code. A zero would say it compiled, so it is
\ refused here rather than recorded.
: REFUSED! ( n n -- ) {: k:n rc:n :}
   k OK drop
   rc 0= if E-JUDGE-ROW-STATE throw then
   rc NEW-RC k SLOT ! ;

: REF! ( n n -- ) {: k:n bytes:n :}
   k OK drop
   bytes REF-BYTES k SLOT ! ;

\ ---- what a reader asks ------------------------------------------------------

: OLD-BYTES@ ( n -- n ) {: k:n :}  OLD-BYTES k OK SLOT @ ;
: NEW-BYTES@ ( n -- n ) {: k:n :}  NEW-BYTES k OK SLOT @ ;
: NEW-RC@ ( n -- n ) {: k:n :}     NEW-RC k OK SLOT @ ;
: REF-BYTES@ ( n -- n ) {: k:n :}  REF-BYTES k OK SLOT @ ;

: REFUSED? ( n -- bool ) {: k:n :}
   k NEW-RC@ 0<> ;

: COVERED? ( n -- bool ) {: k:n :}
   k REF-BYTES@ NO-REFERENCE <> ;

\ ---- the verdict -------------------------------------------------------------

0 constant V-SMALLER
1 constant V-EQUAL
2 constant V-LARGER
3 constant V-REFUSED

: VERDICT ( n -- n ) {: k:n :}
   k REFUSED? if V-REFUSED exit then
   k NEW-BYTES@ k OLD-BYTES@ < if V-SMALLER exit then
   k NEW-BYTES@ k OLD-BYTES@ = if V-EQUAL exit then
   V-LARGER ;

: VERDICT$ ( n -- ptr u8 n ) {: v:n :}
   v V-SMALLER = if s" smaller" exit then
   v V-EQUAL = if s" equal" exit then
   v V-LARGER = if s" LARGER" exit then
   v V-REFUSED = if s" REFUSED" exit then
   E-JUDGE-ROW-STATE throw ;

\ How many rows the chain compiled into more bytes than the engine's emitter
\ wrote for the same body. The one verdict a run exits non-zero on.
: LARGER-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i VERDICT V-LARGER = if 1+ then
   loop ;

: REFUSED-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i REFUSED? if 1+ then
   loop ;

;package
