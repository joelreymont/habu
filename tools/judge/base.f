\ judge/base.f - reading a committed judge artifact back as rows, and saying
\ which DIRECTION each column moved. One concern: what changed since the
\ artifact was pinned, and which of those changes is a finding.
\
\ WHY A SECOND READER OF A FILE THAT IS ALREADY COMPARED BYTE FOR BYTE.
\ tools/judge/check.f answers one question - are the checked halves the same
\ bytes - and that question is the right gate: every column of the checked half
\ is exact, so any movement is something somebody did on purpose and the
\ artifact is regenerated in the same change. What it cannot say is WHICH WAY a
\ number went. A chain row that grew is a regression against ourselves; one that
\ shrank is progress. Both come out of the byte comparison as one sentence -
\ "the tree and the artifact disagree at byte 2317" - and a reader has to open
\ the diff to learn which of the two happened. The comparison this judge
\ replaced adjudicated those two events apart (its baseline reader's
\ SIZE-CHECK, and the IMPROVED note beside it), and that adjudication is what
\ this file carries over now that the harness is gone.
\
\ IT DOES NOT WEAKEN THE GATE AND IS NOT MEANT TO. An improvement still moves
\ the artifact and still exits non-zero, because an artifact that drifted behind
\ the tree would widen the band a later regression could hide in. What changes
\ is what the run SAYS: the rows that moved, named, each with its direction.
\
\ THE READING IS STRUCTURAL, NOT A SEARCH. The checked half is split into lines
\ and a line opens a row only when its FIRST word is a qualified subject name -
\ exactly one colon, at neither end. Prose is everything else, so the subject's
\ name written inside a sentence reads as the sentence it is in, and `rows:`
\ ends in its colon, so the tally cannot open a row however its numbers read. A
\ line that opens a row and then does not carry a row's fields is not stored,
\ and the subject this run measured for it is then reported as a row the
\ artifact has no row for.
\
\ ONLY THE CHECKED HALF IS READ. The costs under the marker line are
\ nanoseconds with a decimal point in them and are measurements of one host;
\ reading them here would be reading a number the artifact does not claim. The
\ cut is this file's, and tools/judge/check.f takes it from here rather than
\ keeping a second copy of where an artifact ends.
\
\ WHICH COLUMN IS ADJUDICATED WHICH WAY, and why the two differ.
\
\   old     the emitter bin/hb ships does not change under this campaign, so a
\           byte count that moved in EITHER direction is a finding: it means the
\           engine's own code moved, or the artifact was pinned against another
\           engine.
\   chain   the column being built. Bigger is a regression against ourselves;
\           smaller is progress, named so the pin is taken deliberately. A row
\           that STARTED being refused lost a capability and is a regression; one
\           that STOPPED being refused gained one and is progress; a refusal
\           whose CODE changed is a different gap under the same word and is a
\           regression, because a gap nobody has named must not read as the one
\           that was named.
\
\ The reference column is not read here at all, and is not in the checked half
\ to be read: what a host's toolchain emits is a fact about that host, no
\ mutation of habu can falsify one of its cells, and a Mach-O host and a Linux
\ one would disagree about every one of them for no habu reason. The artifact
\ prints it below the marker instead (tools/judge/report.f REFERENCE), so a row
\ line here carries the four columns habu's own chain determines and nothing
\ else.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/adt/option.f
require tools/codegen-compare-text.f
require tools/judge/row.f
require tools/judge/report.f

package JUDGE-BASE

private

64 constant CAP-MAX               \ rows one committed artifact holds
48 constant NAME-MAX
$3A constant COLON-BYTE

CAP-MAX NAME-MAX * BUFFER: NAMES
create NAME-LENS CAP-MAX cells allot
create OLD-CELLS CAP-MAX cells allot
create CHAIN-CELLS CAP-MAX cells allot     \ bytes, or the code the chain refused with
create DS-OLD-CELLS CAP-MAX cells allot    \ the caller's data stack, touched
create DS-NEW-CELLS CAP-MAX cells allot    \ NOT-MEASURED for a dash

variable ROW-N
variable REPORT?
variable FINDINGS

: SLOT ( ptr a n -- ptr a )
   cells + ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAMES + ;

: OK ( n -- n )
   dup 0 < over ROW-N @ >= or if E-JUDGE-BASE-ROW throw then ;

\ ---- saying it --------------------------------------------------------------

: SAY ( ptr u8 n -- )
   REPORT? @ if type else 2drop then ;

: SAY-NUM ( n -- )
   REPORT? @ if FMT:.INT else drop then ;

: SAY-END ( -- )
   REPORT? @ if cr then ;

: NOTE ( ptr u8 n -- )
   s" judge: " SAY SAY s"  " SAY ;

: FIND+ ( -- )
   FINDINGS @ 1+ FINDINGS ! ;

\ ---- the row store ----------------------------------------------------------

: ROW-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k OK NAME-AT
   NAME-LENS k SLOT @ ;

: KEEP-ROW ( ptr u8 n n n n n -- )
   {: a:ptr u:n old:n chain:n ds-old:n ds-new:n :}
   ROW-N @ CAP-MAX >= if E-JUDGE-BASE-CAP throw then
   u NAME-MAX > if E-JUDGE-BASE-CAP throw then
   a  ROW-N @ NAME-AT  u STR-LEN BYTE-COPY-LEN
   u NAME-LENS ROW-N @ SLOT !
   old OLD-CELLS ROW-N @ SLOT !
   chain CHAIN-CELLS ROW-N @ SLOT !
   ds-old DS-OLD-CELLS ROW-N @ SLOT !
   ds-new DS-NEW-CELLS ROW-N @ SLOT !
   ROW-N @ 1+ ROW-N ! ;

\ ---- reading one line -------------------------------------------------------

: COLONS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   u 0 ?do i a + c@ COLON-BYTE = if 1+ then loop ;

\ A subject name as the judge spells one: a package qualifier, one colon, a
\ tail. `rows:` ends in its colon and `clang` carries none, so neither line can
\ open a row however its numbers read.
: QUALIFIED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 3 < if false exit then
   a u COLONS 1 <> if false exit then
   a c@ COLON-BYTE = if false exit then
   u 1- a + c@ COLON-BYTE <> ;

\ A cell that carries a number, or the dash a column with nothing to report
\ writes. Since the reference column left the checked half the only dash a row
\ line can still carry is the chain's stack column on a refused row, which has
\ no routine to count - so the dash reads as NOT-MEASURED, which is the
\ constant the traffic checks below compare against.
: CELL-OF ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u s" -" STR= if JUDGE-ROW:NOT-MEASURED true exit then
   a u STR>NUMBER? MATCH option
     none OF 0 false ENDOF
     some OF true ENDOF
   ;MATCH ;

: NEXT-CELL ( -- n bool )
   CODEGEN-TEXT:NEXT$ 0= if 2drop 0 false exit then
   CELL-OF ;

\ Everything after the name, in the order the report writes it. A field that is
\ not what a row carries there abandons the line and stores nothing, so the
\ subject this run measured for it has no partner in the artifact and is
\ reported as a row the artifact does not have. A line that is not a row does
\ not become one by being adjudicated half way.
: PARSE-TAIL ( ptr u8 n -- ) {: na:ptr nu:n :}
   CODEGEN-TEXT:NEXT-NUMBER 0= if drop exit then {: old:n :}
   CODEGEN-TEXT:NEXT-NUMBER 0= if drop exit then {: chain:n :}
   NEXT-CELL 0= if drop exit then {: ds-old:n :}
   NEXT-CELL 0= if drop exit then {: ds-new:n :}
   na nu old chain ds-old ds-new KEEP-ROW ;

: SCAN-LINE ( ptr u8 n -- ) {: la:ptr lu:n :}
   la lu CODEGEN-TEXT:LINE!
   CODEGEN-TEXT:NEXT$ 0= if 2drop exit then {: na:ptr nu:n :}
   na nu QUALIFIED? 0= if exit then
   na nu PARSE-TAIL ;

: SCAN ( ptr u8 n -- )
   CODEGEN-TEXT:TEXT!
   begin
      CODEGEN-TEXT:NEXT-LINE 0= if 2drop exit then
      SCAN-LINE
   again ;

public

: LOUD! ( -- )    -1 REPORT? ! ;
: QUIET! ( -- )   0 REPORT? ! ;

\ Where a rendered or committed artifact's CHECKED half ends: everything before
\ the marker line. A text with no marker is refused rather than compared whole,
\ because a truncated artifact would otherwise be checked against a prefix of
\ itself and pass.
: MARK-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   JUDGE-REPORT:MARK-TEXT$ {: ma:ptr mu:n :}
   -1
   u mu < if 0 else u mu - 1+ then 0 ?do
      dup 0 < if
         a i + mu ma mu STR= if drop i then
      then
   loop
   dup 0 < if E-JUDGE-CHECK-MARK throw then ;

: CHECKED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u MARK-AT {: at:n :}
   a at ;

\ Read an artifact's checked half into rows. The text is a parameter, so the
\ fixtures that attack this reader hand it whole artifacts the same way
\ tools/judge/check.f hands it the committed one.
: LOAD-FROM ( ptr u8 n -- )
   0 ROW-N !
   0 FINDINGS !
   CHECKED$ SCAN ;

: ROWS ( -- n )         ROW-N @ ;

: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   ROW-N @ 0 ?do
      i ROW-NAME$ a u STR= if drop i then
   loop ;

: OLD@ ( n -- n ) {: k:n :}    OLD-CELLS k OK SLOT @ ;
: CHAIN@ ( n -- n ) {: k:n :}  CHAIN-CELLS k OK SLOT @ ;
: DS-OLD@ ( n -- n ) {: k:n :} DS-OLD-CELLS k OK SLOT @ ;
: DS-NEW@ ( n -- n ) {: k:n :} DS-NEW-CELLS k OK SLOT @ ;

\ A committed chain cell holds bytes, or the negative code the chain refused the
\ subject with. Nothing else can stand there, so the sign is the whole test.
: REFUSED@ ( n -- bool ) {: k:n :}
   k CHAIN@ 0 < ;

private

variable REGRESSED
variable IMPROVED
variable ENGINE-MOVED
variable LOST-N
variable GAINED-N

: SAY-DELTA ( ptr u8 n n n -- ) {: a:ptr u:n got:n had:n :}
   a u SAY
   s"  is " SAY got SAY-NUM
   s" , the artifact says " SAY had SAY-NUM
   SAY-END ;

: REGRESSION ( ptr u8 n ptr u8 n n n -- )
   {: la:ptr lu:n a:ptr u:n got:n had:n :}
   la lu NOTE
   a u got had SAY-DELTA
   REGRESSED @ 1+ REGRESSED !
   FIND+ ;

: IMPROVEMENT ( ptr u8 n ptr u8 n n n -- )
   {: la:ptr lu:n a:ptr u:n got:n had:n :}
   la lu NOTE
   a u got had SAY-DELTA
   IMPROVED @ 1+ IMPROVED ! ;

\ What this run's chain column holds for a row, in the one spelling the artifact
\ writes it in: bytes, or the refusal code.
: MEASURED-CHAIN ( n -- n ) {: k:n :}
   k JUDGE-ROW:REFUSED? if k JUDGE-ROW:NEW-RC@ exit then
   k JUDGE-ROW:NEW-BYTES@ ;

: CHAIN-CHECK ( n n -- ) {: k:n b:n :}
   k MEASURED-CHAIN {: got:n :}
   b CHAIN@ {: had:n :}
   got had = if exit then
   k JUDGE-ROW:NAME$ {: a:ptr u:n :}
   had 0 < got 0 < and if
      s" REFUSAL-CHANGED" a u got had REGRESSION exit
   then
   got 0 < if s" NOW-REFUSED" a u got had REGRESSION exit then
   had 0 < if s" NO-LONGER-REFUSED" a u got had IMPROVEMENT exit then
   got had > if s" LARGER-THAN-ARTIFACT" a u got had REGRESSION exit then
   s" SMALLER-THAN-ARTIFACT" a u got had IMPROVEMENT ;

: OLD-CHECK ( n n -- ) {: k:n b:n :}
   k JUDGE-ROW:OLD-BYTES@ {: got:n :}
   b OLD@ {: had:n :}
   got had = if exit then
   s" ENGINE-MOVED" NOTE
   k JUDGE-ROW:NAME$ got had SAY-DELTA
   ENGINE-MOVED @ 1+ ENGINE-MOVED !
   FIND+ ;

\ The chain's data-stack traffic, adjudicated the way its bytes are: more is a
\ regression against ourselves and fewer is progress. A side with no routine to
\ count has nothing to compare, and neither has one whose partner has none.
: CHAIN-TRAFFIC-CHECK ( n n -- ) {: k:n b:n :}
   k JUDGE-ROW:NEW-TRAFFIC@ {: got:n :}
   b DS-NEW@ {: had:n :}
   got JUDGE-ROW:NOT-MEASURED = had JUDGE-ROW:NOT-MEASURED = or if exit then
   got had = if exit then
   k JUDGE-ROW:NAME$ {: a:ptr u:n :}
   got had > if s" MORE-STACK-THAN-ARTIFACT" a u got had REGRESSION exit then
   s" LESS-STACK-THAN-ARTIFACT" a u got had IMPROVEMENT ;

: OLD-TRAFFIC-CHECK ( n n -- ) {: k:n b:n :}
   k JUDGE-ROW:OLD-TRAFFIC@ {: got:n :}
   b DS-OLD@ {: had:n :}
   got JUDGE-ROW:NOT-MEASURED = had JUDGE-ROW:NOT-MEASURED = or if exit then
   got had = if exit then
   s" ENGINE-STACK-MOVED" NOTE
   k JUDGE-ROW:NAME$ got had SAY-DELTA
   ENGINE-MOVED @ 1+ ENGINE-MOVED !
   FIND+ ;

: MEASURED-ROW-CHECK ( n -- ) {: k:n :}
   k JUDGE-ROW:NAME$ FIND {: b:n :}
   b 0 < if
      s" NEW-ROW" NOTE
      k JUDGE-ROW:NAME$ SAY
      s"  was measured and the artifact has no row for it" SAY SAY-END
      GAINED-N @ 1+ GAINED-N !
      FIND+ exit
   then
   k b OLD-CHECK
   k b CHAIN-CHECK
   k b OLD-TRAFFIC-CHECK
   k b CHAIN-TRAFFIC-CHECK ;

: COMMITTED-ROW-CHECK ( n -- ) {: b:n :}
   b ROW-NAME$ JUDGE-ROW:FIND 0 >= if exit then
   s" LOST-ROW" NOTE
   b ROW-NAME$ SAY
   s"  is in the artifact and this run did not measure it" SAY SAY-END
   LOST-N @ 1+ LOST-N !
   FIND+ ;

public

\ Adjudicate the rows JUDGE-ROW just measured against the artifact LOAD-FROM
\ read. Counts each direction apart and answers the findings: everything but an
\ improvement and a moved reference column.
: ADJUDICATE ( -- n )
   0 REGRESSED !
   0 IMPROVED !
   0 ENGINE-MOVED !
   0 LOST-N !
   0 GAINED-N !
   JUDGE-ROW:ROWS 0 ?do i MEASURED-ROW-CHECK loop
   ROW-N @ 0 ?do i COMMITTED-ROW-CHECK loop
   FINDINGS @ ;

: REGRESSIONS ( -- n )    REGRESSED @ ;
: IMPROVEMENTS ( -- n )   IMPROVED @ ;
: ENGINE-MOVES ( -- n )   ENGINE-MOVED @ ;
: LOST ( -- n )           LOST-N @ ;
: GAINED ( -- n )         GAINED-N @ ;

private

: INIT ( -- )
   -1 REPORT? !
   0 ROW-N !
   0 FINDINGS ! ;

INIT

;package
