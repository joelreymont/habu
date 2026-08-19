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
\ WHY THE COMPARED TABLE IS COUNTS AND NOT COSTS. A byte count is read off the
\ word's own dictionary record, and the data-stack traffic beside it is counted
\ in the same emitted code: both are the same number on every host, in every
\ run, moving only when a compiler moves them, so both can be compared exactly
\ against a committed artifact. A cost is a measurement, and a machine with
\ every core busy - which is what a gate is - moves one by more than any honest
\ tolerance would catch, so the costs are held here but never written into the
\ artifact. What is asked of them is a DIRECTION, at the foot of this file,
\ against a band this run measured for itself; the entry that asserts it is
\ tools/judge-timed.f and no suite schedules it.

require lib/errors.f
require lib/prelude.f
require lib/string.f

package JUDGE-ROW

private

64 constant CAP-MAX               \ subjects one judged table holds
48 constant NAME-MAX
1000 constant PERMILLE            \ the unit a measured noise gap is kept in

CAP-MAX NAME-MAX * BUFFER: NAMES
create NAME-LENS CAP-MAX cells allot
create OLD-BYTES CAP-MAX cells allot
create NEW-BYTES CAP-MAX cells allot
create NEW-RC CAP-MAX cells allot          \ 0, or the code the chain refused with
create REF-BYTES CAP-MAX cells allot       \ -1 when the subject has no C twin
create OLD-PICOS CAP-MAX cells allot
create NEW-PICOS CAP-MAX cells allot
create REF-PICOS CAP-MAX cells allot
create REF-FLOOR CAP-MAX cells allot       \ the twin's own entry, same arity, empty body
create OLD-VALUE CAP-MAX cells allot
create NEW-VALUE CAP-MAX cells allot
create REF-VALUE CAP-MAX cells allot
create OLD-WIT CAP-MAX cells allot         \ what the memory held after the call
create NEW-WIT CAP-MAX cells allot
create REF-WIT CAP-MAX cells allot
create HAS-WIT CAP-MAX cells allot         \ this row has a reader for that memory
create OUTS CAP-MAX cells allot            \ how many values the subject leaves
create NEW-TAIL CAP-MAX cells allot        \ the chain's routine leaves by a branch
create ALT-BAD CAP-MAX cells allot         \ a column disagreed on an input past the first
create IN-COUNT CAP-MAX cells allot        \ pinned inputs this row is valued on
create REF-INPUTS CAP-MAX cells allot      \ of those, how many the C twin was valued on too
create OLD-TRAFFIC CAP-MAX cells allot     \ accesses to the caller's data stack
create NEW-TRAFFIC CAP-MAX cells allot     \ NOT-MEASURED when the chain refused the row

variable FILL
variable WORST-SPREAD              \ permille, the worst gap between two measurements of one program
variable FLOOR-CELL                \ what every habu column's cost carries in common

: SLOT ( ptr a n -- ptr a )
   cells + ;

: OK ( n -- n )
   dup 0 < over FILL @ >= or if E-JUDGE-ROW-INDEX throw then ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAMES + ;

public

-1 constant NO-REFERENCE           \ this subject has no C twin
-1 constant NOT-MEASURED           \ no routine of this column exists to read

: RESET ( -- )
   0 FILL !
   0 WORST-SPREAD !
   0 FLOOR-CELL ! ;

\ ---- the measurement's own floor and its measured noise ----------------------

\ The cost every habu column carries in common - the timing loop, the call into
\ the quotation and the return - measured over an empty quotation. Subtracting
\ it leaves the cost of the one call a row's body makes.
: FLOOR! ( n -- )
   FLOOR-CELL ! ;

: FLOOR@ ( -- n )
   FLOOR-CELL @ ;

\ Keep the faster of two measurements of ONE program, and remember how far
\ apart they were. That gap is not an estimate of the noise on this host, it is
\ the noise this run actually saw: the same body, the same inputs, the same
\ process, measured twice with the other columns in between. A run reports its
\ worst, so a reader of a cost column knows what a difference has to exceed
\ before it is a difference.
: SAMPLE ( n n -- n ) {: had:n got:n :}
   had 0= if got exit then
   had got < if had else got then {: lo:n :}
   had got - dup 0 < if negate then {: gap:n :}
   lo 0 > if
      gap PERMILLE * lo / {: permille:n :}
      permille WORST-SPREAD @ > if permille WORST-SPREAD ! then
   then
   lo ;

: SPREAD@ ( -- n )
   WORST-SPREAD @ ;

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
   0 OLD-PICOS FILL @ SLOT !
   0 NEW-PICOS FILL @ SLOT !
   0 REF-PICOS FILL @ SLOT !
   0 REF-FLOOR FILL @ SLOT !
   0 OLD-VALUE FILL @ SLOT !
   0 NEW-VALUE FILL @ SLOT !
   0 REF-VALUE FILL @ SLOT !
   0 OLD-WIT FILL @ SLOT !
   0 NEW-WIT FILL @ SLOT !
   0 REF-WIT FILL @ SLOT !
   0 HAS-WIT FILL @ SLOT !
   1 OUTS FILL @ SLOT !
   0 NEW-TAIL FILL @ SLOT !
   0 ALT-BAD FILL @ SLOT !
   1 IN-COUNT FILL @ SLOT !
   0 REF-INPUTS FILL @ SLOT !
   NOT-MEASURED OLD-TRAFFIC FILL @ SLOT !
   NOT-MEASURED NEW-TRAFFIC FILL @ SLOT !
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

\ A column's cost and the answer it computed. The two arrive together because a
\ time without the answer that program produced is a number about an unknown
\ program: tools/judge/cost.f builds both bodies from one input text, and a
\ column whose answer differs from another's was not running the row.
: OLD-COST! ( n n n -- ) {: k:n picos:n value:n :}
   k OK drop
   picos OLD-PICOS k SLOT !
   value OLD-VALUE k SLOT ! ;

: NEW-COST! ( n n n -- ) {: k:n picos:n value:n :}
   k OK drop
   picos NEW-PICOS k SLOT !
   value NEW-VALUE k SLOT ! ;

: REF-COST! ( n n n n -- ) {: k:n picos:n floor:n value:n :}
   k OK drop
   picos REF-PICOS k SLOT !
   floor REF-FLOOR k SLOT !
   value REF-VALUE k SLOT ! ;

\ ---- the memory a subject wrote ----------------------------------------------
\ A second number per column, read back through the row's own reader after the
\ same call. It is kept apart from the answer rather than folded into it: a
\ subject's answer and the memory it wrote are two observations, and one cell
\ holding both lets a column that got both wrong the same way cancel out.
\ CODEGEN-CORPUS:CELL-BUMP answers exactly what its cell holds, so folded
\ together the two are identically zero and prove nothing; compared separately
\ neither can hide the other.

: OLD-WITNESS! ( n n -- ) {: k:n value:n :}
   k OK drop
   value OLD-WIT k SLOT !
   1 HAS-WIT k SLOT ! ;

: NEW-WITNESS! ( n n -- ) {: k:n value:n :}
   k OK drop
   value NEW-WIT k SLOT ! ;

: REF-WITNESS! ( n n -- ) {: k:n value:n :}
   k OK drop
   value REF-WIT k SLOT ! ;

: WITNESSED? ( n -- bool ) {: k:n :}
   HAS-WIT k OK SLOT @ 0<> ;

: OLD-WITNESS@ ( n -- n ) {: k:n :}  OLD-WIT k OK SLOT @ ;
: NEW-WITNESS@ ( n -- n ) {: k:n :}  NEW-WIT k OK SLOT @ ;

private

\ Only the agreement check below reads it: nothing outside compares a twin's
\ memory with the engine's.
: REF-WITNESS@ ( n -- n ) {: k:n :}  REF-WIT k OK SLOT @ ;

public

\ The chain compiled it, into this many bytes.
: NEW! ( n n bool -- ) {: k:n bytes:n tail:bool :}
   k OK drop
   0 NEW-RC k SLOT !
   bytes NEW-BYTES k SLOT !
   tail if 1 else 0 then  NEW-TAIL k SLOT  ! ;

\ Does the chain's routine for this row leave by a branch to a callee? Then the
\ bytes recorded here are the routine and not the whole program: the callee is
\ real and its bytes are somewhere else.
: NEW-TAIL? ( n -- bool ) {: k:n :}
   NEW-TAIL k OK SLOT @ 0<> ;

: TAIL-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i NEW-TAIL? if 1+ then
   loop ;

\ The chain refused it, with this code. A zero would say it compiled, so it is
\ refused here rather than recorded.
: REFUSED! ( n n -- ) {: k:n rc:n :}
   k OK drop
   rc 0= if E-JUDGE-ROW-STATE throw then
   rc NEW-RC k SLOT ! ;

: REF! ( n n -- ) {: k:n bytes:n :}
   k OK drop
   bytes REF-BYTES k SLOT ! ;

\ ---- the pinned inputs past the first ----------------------------------------
\ A row states one input its cost is measured on and may state more, and the
\ others exist for the arms the first does not take: an empty span, a miss, a
\ zero, a NaN, each rung of a ladder. Their answers are not recorded - what they
\ establish is that independently compiled programs still agree - so all a row
\ keeps of them is HOW MANY it was valued on, how many of those the C twin was
\ valued on too, and WHETHER any of them disagreed.
\
\ WHY THE REFERENCE COUNT IS KEPT APART. A twin is optional per row already: a
\ subject with no C symbol carries a dash in the byte column and a host with no
\ compiler has no reference column at all. The same holds one input at a time -
\ a row may state an input the twin has no counterpart for, over a buffer the C
\ file does not carry - so the two habu columns are compared on EVERY tuple and
\ the reference on the ones it can reach. The tally prints both counts, because
\ a reference comparison silently not made is indistinguishable from one made
\ and passed.

: INPUTS! ( n n -- ) {: k:n n-in:n :}
   k OK drop
   n-in IN-COUNT k SLOT ! ;

: INPUTS@ ( n -- n ) {: k:n :}
   IN-COUNT k OK SLOT @ ;

: REF-INPUTS! ( n n -- ) {: k:n n-in:n :}
   k OK drop
   n-in REF-INPUTS k SLOT ! ;

: REF-INPUTS@ ( n -- n ) {: k:n :}
   REF-INPUTS k OK SLOT @ ;

\ Every pinned input this run valued, over the whole table, and the ones the
\ reference column reached. The artifact prints both, so an input quietly
\ dropped from a corpus moves the committed file.
: TOTAL-INPUTS ( -- n )
   0
   FILL @ 0 ?do
      i INPUTS@ +
   loop ;

: TOTAL-REF-INPUTS ( -- n )
   0
   FILL @ 0 ?do
      i REF-INPUTS@ +
   loop ;

: ALT-BAD! ( n -- ) {: k:n :}
   k OK drop
   1 ALT-BAD k SLOT ! ;

private

: ALT-BAD? ( n -- bool ) {: k:n :}
   ALT-BAD k OK SLOT @ 0<> ;

public

\ ---- what each habu column spends on the caller's data stack -----------------
\ An exact count off the emitted code, so it belongs in the checked half of the
\ artifact beside the byte columns rather than beside the costs. The reference
\ column has no such number: a C function keeps its intermediates where its own
\ compiler put them and never touches this stack at all.

: OLD-TRAFFIC! ( n n -- ) {: k:n n-acc:n :}
   k OK drop
   n-acc OLD-TRAFFIC k SLOT ! ;

: NEW-TRAFFIC! ( n n -- ) {: k:n n-acc:n :}
   k OK drop
   n-acc NEW-TRAFFIC k SLOT ! ;

: OLD-TRAFFIC@ ( n -- n ) {: k:n :}  OLD-TRAFFIC k OK SLOT @ ;
: NEW-TRAFFIC@ ( n -- n ) {: k:n :}  NEW-TRAFFIC k OK SLOT @ ;

\ Does the chain's routine touch the caller's data stack more often than the
\ engine's code for the same body? A refused row has no routine to count, so it
\ is not heavier and not lighter - it is not measured.
: HEAVIER? ( n -- bool ) {: k:n :}
   k NEW-TRAFFIC@ NOT-MEASURED = if false exit then
   k OLD-TRAFFIC@ NOT-MEASURED = if false exit then
   k NEW-TRAFFIC@ k OLD-TRAFFIC@ > ;

: HEAVIER-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i HEAVIER? if 1+ then
   loop ;

\ ---- what a reader asks ------------------------------------------------------

: OLD-BYTES@ ( n -- n ) {: k:n :}  OLD-BYTES k OK SLOT @ ;
: NEW-BYTES@ ( n -- n ) {: k:n :}  NEW-BYTES k OK SLOT @ ;
: NEW-RC@ ( n -- n ) {: k:n :}     NEW-RC k OK SLOT @ ;
: REF-BYTES@ ( n -- n ) {: k:n :}  REF-BYTES k OK SLOT @ ;

: OLD-PICOS@ ( n -- n ) {: k:n :}  OLD-PICOS k OK SLOT @ ;
: NEW-PICOS@ ( n -- n ) {: k:n :}  NEW-PICOS k OK SLOT @ ;
: REF-PICOS@ ( n -- n ) {: k:n :}  REF-PICOS k OK SLOT @ ;
: REF-FLOOR@ ( n -- n ) {: k:n :}  REF-FLOOR k OK SLOT @ ;
: OLD-VALUE@ ( n -- n ) {: k:n :}  OLD-VALUE k OK SLOT @ ;
: NEW-VALUE@ ( n -- n ) {: k:n :}  NEW-VALUE k OK SLOT @ ;
: REF-VALUE@ ( n -- n ) {: k:n :}  REF-VALUE k OK SLOT @ ;

\ Does the subject leave more than one value? Recorded when the row is opened,
\ because it decides whether the reference column can be compared at all.
: MULTI? ( n -- bool ) {: k:n :}
   OUTS k OK SLOT @ 1 > ;

: OUTS! ( n n -- ) {: k:n n-outs:n :}
   k OK drop
   n-outs  OUTS k SLOT  ! ;

: REFUSED? ( n -- bool ) {: k:n :}
   k NEW-RC@ 0<> ;

: COVERED? ( n -- bool ) {: k:n :}
   k REF-BYTES@ NO-REFERENCE <> ;

\ ---- the verdict -------------------------------------------------------------
\ The four are the store's own spelling of a verdict. A caller reads a row's
\ verdict through VERDICT and renders it through VERDICT$, so the numbers stay
\ inside.

private

0 constant V-SMALLER
1 constant V-EQUAL
2 constant V-LARGER
3 constant V-REFUSED

public

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

\ ---- do the columns compute the same thing? ----------------------------------
\ Every column that ran is checked against the engine's answer on the same
\ pinned input. A refused column never ran and is not compared; a subject with
\ no C twin has no reference answer to compare. A disagreement means one of the
\ generated bodies is not the program the row is about, so neither its time nor
\ its bytes mean anything - it is the one condition that makes a whole row
\ worthless rather than merely worse. What a caller asks is DISAGREEING-ROWS;
\ the three predicates it is made of stay inside.

private

: NEW-AGREES? ( n -- bool ) {: k:n :}
   k REFUSED? if true exit then
   k NEW-VALUE@ k OLD-VALUE@ <> if false exit then
   k WITNESSED? 0= if true exit then
   k NEW-WITNESS@ k OLD-WITNESS@ = ;

\ A reference row's ANSWER is what a FOREIGN call answers, which is one value
\ however many the habu subject leaves. A subject that leaves more than one is
\ compared across the two habu columns and not against the twin: there is no
\ single number on the reference side to compare it with, and folding a C
\ function's one result against a fold of two habu results would be comparing two
\ different things and calling their difference a finding. Its MEMORY is a
\ different matter - the twin writes its own copy of the same data and the row's
\ reader reads that copy back - so a witness is compared however many values the
\ subject left.
: REF-AGREES? ( n -- bool ) {: k:n :}
   k COVERED? 0= if true exit then
   k WITNESSED? if
      k REF-WITNESS@ k OLD-WITNESS@ <> if false exit then
   then
   k MULTI? if true exit then
   k REF-VALUE@ k OLD-VALUE@ = ;

\ An input past the first is compared and not recorded, so what it leaves behind
\ is one flag. A row that disagreed on ANY of its inputs disagrees.
: AGREES? ( n -- bool ) {: k:n :}
   k ALT-BAD? if false exit then
   k NEW-AGREES? k REF-AGREES? and ;

public

: DISAGREEING-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i AGREES? 0= if 1+ then
   loop ;

\ How many rows the chain compiled into more bytes than the engine's emitter
\ wrote for the same body. The one verdict a run exits non-zero on.
: LARGER-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i VERDICT V-LARGER = if 1+ then
   loop ;

\ ---- is the chain's code slower than the engine's? ---------------------------
\ THE CLAIM THE WHOLE COMPARISON EXISTS TO MAKE, and the one nothing above can
\ carry: a byte count is not a cost, and a compiler that emitted fewer, slower
\ instructions would pass every verdict in this file. It is answered here and
\ asserted by tools/judge-timed.f, which no suite schedules.
\
\ WHAT MAKES IT A DIRECTION AND NOT A NUMBER. Both columns are measured in ONE
\ pass, in one process, interleaved, with the same floor taken off - the two
\ habu columns are entered the same way, so they lose the same constant and
\ their difference is exact. So nothing is committed and nothing is compared
\ across hosts: the engine's own cost for the same row IS the calibration, and
\ what is claimed is only which of the two is larger.
\
\ AND THE BAND IS MEASURED, NOT CHOSEN. A cost is a measurement, so two columns
\ that differ by less than the noise do not differ. The noise is not estimated
\ here: SAMPLE above measures it, as the widest gap between two measurements of
\ ONE program in this run, and that gap is the band. It widens by itself on a
\ loaded host, because it is measured on that host in the same process - which
\ is the property a fixed tolerance cannot have. The old comparison's
\ COST-BAND, eight, is deliberately NOT reused: it is calibrated for a different
\ question, a live measurement against a number recorded on ANOTHER machine on
\ ANOTHER day, and it has to absorb the difference between two hosts as well as
\ the noise on one. Against two columns of one pass it would admit a chain eight
\ times slower than the engine and call it level.
\
\ A ROW WHOSE ENGINE COST IS AT OR UNDER THE MEASUREMENT'S OWN FLOOR has no
\ ratio to be judged by, and it is counted apart rather than passed quietly: an
\ un-adjudicable row that read as "not slower" is exactly how a claim goes
\ silent.

public

: COST-COMPARABLE? ( n -- bool ) {: k:n :}
   k REFUSED? if false exit then
   k OLD-PICOS@ FLOOR@ - 0 > ;

: UNCOMPARABLE-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i REFUSED? 0= if
         i COST-COMPARABLE? 0= if 1+ then
      then
   loop ;

: SLOWER? ( n -- bool ) {: k:n :}
   k COST-COMPARABLE? 0= if false exit then
   k OLD-PICOS@ FLOOR@ - {: old:n :}
   k NEW-PICOS@ FLOOR@ - {: new:n :}
   new old <= if false exit then
   new old -  PERMILLE *  old /  WORST-SPREAD @ > ;

: SLOWER-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i SLOWER? if 1+ then
   loop ;

: REFUSED-ROWS ( -- n )
   0
   FILL @ 0 ?do
      i REFUSED? if 1+ then
   loop ;

;package
