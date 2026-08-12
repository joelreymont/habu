\ judge/fuzz.f - the differential oracle: straight-line integer programs nobody
\ wrote, compiled by BOTH code generators from one text and required to answer
\ the same cell on every input. One concern: generating a program and holding
\ the two compilations of it against each other.
\
\ WHAT THIS REPLACES. The old comparison records a finite vector of outputs per
\ subject - a handful of inputs somebody chose, written down beside the answers
\ they produced - so what it can catch is bounded by what somebody thought to
\ write. Here the programs are generated, and every one of them is run on the
\ ends of the signed range as well as on generated inputs, so a difference
\ between the two code generators has to survive all of them rather than avoid
\ the few that were recorded.
\
\ THE SEED IS A CONSTANT AND NEVER A CLOCK. A run that read the time would find a
\ different bug every day and none of them twice; this one generates the same
\ programs in the same order on every host, so a failure can be reproduced and a
\ green run is a statement about a fixed set. The gate member runs a small fixed
\ number of them in seconds; the full sweep is the hand-run entry
\ tools/judge-fuzz.f, which is where a larger number belongs.
\
\ ONE TEXT, TWO COMPILERS, AND THE READER BETWEEN THEM. The generator writes ONE
\ source text. tools/judge/src.f reads it - the same reader every corpus goes
\ through - and derives the two copies by suffix, so neither compiler is handed a
\ hand-copied second opinion about what the program is. The engine compiles its
\ copy through tools/judge/cost.f's audited evaluate boundary, which is the path
\ every generated body in this tool already takes; the chain compiles its copy
\ through src/compiler/native/migrate.f, which is the path every corpus subject
\ takes.
\
\ ONE DRIVER PER COLUMN, NOT ONE PER INPUT. The inputs are CELLS in a table here,
\ and each column is called by a single generated body that walks that table and
\ writes its answers into another. Every generated word is a dictionary record
\ that is never reclaimed, so a definition per input would make the size of a
\ sweep a question about the dictionary rather than about the compilers; and the
\ inputs being cells rather than text means the ends of the signed range are just
\ values, with no spelling for anybody to get wrong.
\
\ THE ANSWERS ARE COMPARED AS CELLS, so what is compared is a program's whole
\ sixty-four-bit answer and not a printed approximation of it.
\
\ WHAT THIS ORACLE CANNOT SEE, STATED PLAINLY. Both columns read ONE text. A
\ generator that emitted a program nobody meant - or a reader that derived the
\ wrong one - hands the SAME wrong program to both compilers, they agree about
\ it, and no differential test can notice. That is a shared-mode failure and it
\ is the honest boundary of this oracle's coverage: it proves the two code
\ generators agree, never that the program they agree about is the one intended.
\ What guards the shared half is elsewhere - tools/judge/src-test.f attacks the
\ reader with sources built to fool it, and the corpora are real programs with
\ committed answers.
\
\ A REFUSAL IS A FAILURE HERE, unlike in the judged table. The generator emits
\ straight-line integer bodies inside the dialect the chain compiles, so a code
\ coming back means either the generator left that dialect or the chain lost a
\ capability, and both are findings. The body and the code are printed with it.
\
\ AND THE TWO COLUMNS MUST BE TWO ROUTINES. Before any answer is compared, the
\ two derived words are resolved through the chain's own resolver and required to
\ start at two different non-zero addresses. A pair of names that resolved to one
\ routine would agree about everything and prove nothing at all.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require src/compiler/native/dict.f
require tools/judge/src.f
require tools/judge/chain.f
require tools/judge/cost.f

package JUDGE-FUZZ

private

$800 constant SRC-CAP             \ bytes of one generated source text
$100 constant DRV-CAP             \ bytes of one generated driver body
$40 constant NAME-CAP             \ bytes of one derived or driver name
64 constant IN-MAX                \ inputs one program is run on
5 constant DEPTH-MAX              \ deepest the generated stack is allowed to get
12 constant STEP-MAX              \ operations in one generated body

create SRC SRC-CAP allot
create DRV DRV-CAP allot
create SUB-NAME NAME-CAP allot     \ the subject the generated source defines
create ENG-NAME NAME-CAP allot     \ the engine's derived word
create CHN-NAME NAME-CAP allot     \ the chain's derived word
create DRV-NAME NAME-CAP allot     \ the body that calls one column on every input
variable SRC-U
variable DRV-U
variable SUB-NAME-U
variable ENG-NAME-U
variable CHN-NAME-U
variable DRV-NAME-U

create IN-VALS IN-MAX cells allot   \ the inputs a program is run on
create ANS IN-MAX cells allot        \ where a generated driver leaves its answers
create ENG-ANS IN-MAX cells allot    \ what the engine's column answered
variable IN-N

variable STATE                    \ the generator's state
variable SEQ                      \ how many bodies this process has generated
variable DEPTH                    \ the generated stack's depth as the body is built
variable BODIES-CELL
variable CHECKS-CELL
variable MISMATCH-CELL

\ A constant, chosen once and written down: the odd multiplier SplitMix64 uses.
\ Nothing in this file reads a clock, an address or an environment.
$2545F4914F6CDD1D constant SEED

\ The two ends of the signed range, as values. They are cells here and never
\ text, so nothing has to spell the one integer whose magnitude has no positive
\ decimal form.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

: PUT ( ptr u8 n ptr u8 ptr a n -- ) {: a:ptr u:n dst:ptr lenp:ptr cap:n :}
   u cap > if E-JUDGE-FUZZ-CAP throw then
   a dst u STR-LEN BYTE-COPY-LEN
   u lenp ! ;

: ADD ( ptr u8 n ptr u8 ptr a n -- ) {: a:ptr u:n dst:ptr lenp:ptr cap:n :}
   lenp @ u + cap > if E-JUDGE-FUZZ-CAP throw then
   a  dst lenp @ +  u STR-LEN BYTE-COPY-LEN
   lenp @ u + lenp ! ;

: SRC+ ( ptr u8 n -- )
   SRC SRC-U SRC-CAP ADD ;

: DRV+ ( ptr u8 n -- )
   DRV DRV-U DRV-CAP ADD ;

: SUB-NAME$ ( -- ptr u8 n )  SUB-NAME SUB-NAME-U @ ;
: ENG-NAME$ ( -- ptr u8 n )  ENG-NAME ENG-NAME-U @ ;
: CHN-NAME$ ( -- ptr u8 n )  CHN-NAME CHN-NAME-U @ ;
: DRV-NAME$ ( -- ptr u8 n )  DRV-NAME DRV-NAME-U @ ;

\ ---- the generator's own arithmetic ------------------------------------------
\ xorshift64: three shifts and three exclusive ors, from a non-zero state, which
\ this one never leaves.

: NEXT ( -- n )
   STATE @
   dup 13 lshift xor
   dup 7 rshift xor
   dup 17 lshift xor
   dup STATE ! ;

\ A value in 0 .. mask, by masking rather than by dividing: every mask this file
\ asks for is one less than a power of two.
: NEXT-MASKED ( n -- n ) {: mask:n :}
   NEXT mask and ;

\ A literal for a generated body: wide enough to need materialising, and never
\ the one integer a decimal spelling cannot carry - a body's literals are TEXT,
\ read back off the chain's tape, unlike the inputs.
: NEXT-LITERAL ( -- n )
   $3FFFFFFFFFFFFFFF NEXT-MASKED {: v:n :}
   1 NEXT-MASKED 0<> if v negate else v then ;

: DEC+ ( n -- ) {: v:n :}
   SB-RESET v FMT:SB-INT SB$ SRC+ ;

\ ---- one generated body -------------------------------------------------------
\ Every step keeps the body straight-line and leaves the checker able to certify
\ it: the depth is tracked here, an operation that would take the stack below one
\ value is never emitted, and the body is closed by folding whatever is left down
\ to the one value its signature declares.

: DEEP? ( -- bool )
   DEPTH @ 2 >= ;

: PUSH-LIT ( -- )
   s"  " SRC+ NEXT-LITERAL DEC+
   DEPTH @ 1+ DEPTH ! ;

: BINARY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SRC+
   DEPTH @ 1- DEPTH ! ;

: LIT-OP ( ptr u8 n -- ) {: a:ptr u:n :}
   PUSH-LIT
   a u BINARY ;

: SHIFT-OP ( ptr u8 n -- ) {: a:ptr u:n :}
   s"  " SRC+ $3F NEXT-MASKED DEC+
   DEPTH @ 1+ DEPTH !
   a u BINARY ;

: DUP-OP ( -- )
   s"  dup" SRC+
   DEPTH @ 1+ DEPTH ! ;

: OVER-OP ( -- )
   s"  over" SRC+
   DEPTH @ 1+ DEPTH ! ;

: SWAP-OP ( -- )
   s"  swap" SRC+ ;

: DROP-OP ( -- )
   s"  drop" SRC+
   DEPTH @ 1- DEPTH ! ;

\ The operations a step chooses from, by four bits of the generator. Nine of the
\ sixteen work on one value and are always available; the other seven need two,
\ and a step that draws one when there is only one emits a `dup` instead.
: STEP-SHALLOW ( n -- ) {: k:n :}
   k 0 = if s"  +" LIT-OP exit then
   k 1 = if s"  -" LIT-OP exit then
   k 2 = if s"  *" LIT-OP exit then
   k 3 = if s"  and" LIT-OP exit then
   k 4 = if s"  or" LIT-OP exit then
   k 5 = if s"  xor" LIT-OP exit then
   k 6 = if s"  lshift" SHIFT-OP exit then
   k 7 = if s"  rshift" SHIFT-OP exit then
   k 8 = if DUP-OP exit then
   PUSH-LIT ;

: STEP-DEEP ( n -- ) {: k:n :}
   k 9 = if OVER-OP exit then
   k 10 = if SWAP-OP exit then
   k 11 = if DROP-OP exit then
   k 12 = if s"  +" BINARY exit then
   k 13 = if s"  xor" BINARY exit then
   s"  *" BINARY ;

\ At the deepest the body is allowed to get, the step is forced to be one that
\ shortens it, so a generated body never asks the register allocator for a depth
\ this file did not intend to measure.
: STEP ( -- )
   $F NEXT-MASKED {: k:n :}
   DEPTH @ DEPTH-MAX >= if s"  xor" BINARY exit then
   k 9 < if k STEP-SHALLOW exit then
   DEEP? 0= if DUP-OP exit then
   k STEP-DEEP ;

: CLOSE ( -- )
   begin DEPTH @ 1 > while s"  xor" BINARY repeat ;

\ The name the generated subject goes by, fresh for every body: a redefinition
\ would leave one dictionary record answering for two programs.
: SUB-NAME! ( -- )
   s" JF" SUB-NAME SUB-NAME-U NAME-CAP PUT
   SB-RESET SEQ @ FMT:SB-U SB$ SUB-NAME SUB-NAME-U NAME-CAP ADD
   SEQ @ 1+ SEQ ! ;

public

\ One generated straight-line integer program, as a whole definition.
: BODY$ ( -- ptr u8 n )
   SUB-NAME!
   1 DEPTH !
   s" : " SRC SRC-U SRC-CAP PUT
   SUB-NAME$ SRC+
   s"  ( n -- n )" SRC+
   STEP-MAX 0 ?do STEP loop
   CLOSE
   s"  ;" SRC+
   SRC SRC-U @ ;

\ Where a generated driver reads its input and leaves its answer. Public because
\ the generated text names them, and guarded because the text is generated: an
\ index outside the table would be this file writing over its own storage rather
\ than a program answering wrongly.
: IN@ ( n -- n ) {: k:n :}
   k 0 < k IN-N @ >= or if E-JUDGE-FUZZ-INDEX throw then
   IN-VALS k cells + @ ;

: ANS! ( n n -- ) {: v:n k:n :}
   k 0 < k IN-N @ >= or if E-JUDGE-FUZZ-INDEX throw then
   v ANS k cells + ! ;

private

\ ---- compiling one text through each code generator --------------------------
\ Both copies are derived from the text by tools/judge/src.f, which is the reader
\ every corpus goes through, so the two compilers are handed one program under
\ two names rather than two texts somebody kept in step.

: ENGINE-SUFFIX$ ( -- ptr u8 n )   s" -FE" ;
: CHAIN-SUFFIX$ ( -- ptr u8 n )    s" -FC" ;

: SCAN-ONE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 <> if E-JUDGE-FUZZ-SOURCE throw then ;

: COLUMN-NAME! ( ptr u8 n ptr u8 ptr a -- ) {: sa:ptr su:n dst:ptr lenp:ptr :}
   0 JUDGE-SRC:NAME$ dst lenp NAME-CAP PUT
   sa su dst lenp NAME-CAP ADD ;

\ Compile the text through the ENGINE and answer where its routine starts.
: ENGINE-COMPILE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u SCAN-ONE
   0 ENGINE-SUFFIX$ JUDGE-SRC:TEXT$ JUDGE-COST:DEFINE
   ENGINE-SUFFIX$ ENG-NAME ENG-NAME-U COLUMN-NAME!
   ENG-NAME$ NDICT:CALL-TARGET ;

\ Compile the text through the CHAIN and answer where its routine starts.
: CHAIN-COMPILE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u SCAN-ONE
   CHAIN-SUFFIX$ JUDGE-CHAIN:SUFFIX!
   0 JUDGE-CHAIN:PUBLISH {: rc:n :}
   rc 0<> if
      s" judge-fuzz: the chain refused a generated body, code " type rc FMT:.INT cr
      a u type cr
      E-JUDGE-FUZZ-REFUSED throw
   then
   CHAIN-SUFFIX$ CHN-NAME CHN-NAME-U COLUMN-NAME!
   CHN-NAME$ NDICT:CALL-TARGET ;

\ ---- the body that calls one column on every input ----------------------------
\ Compiled by the engine, once per column, and run once: it walks the input table
\ and writes what the column answered into the answer table.
\
\   : JFDn ( -- ) <count> 0 ?do i JUDGE-FUZZ:IN@ <word> i JUDGE-FUZZ:ANS! loop ;

: DRV-NAME! ( -- )
   s" JFD" DRV-NAME DRV-NAME-U NAME-CAP PUT
   SB-RESET SEQ @ FMT:SB-U SB$ DRV-NAME DRV-NAME-U NAME-CAP ADD
   SEQ @ 1+ SEQ ! ;

: DRIVE ( ptr u8 n -- ) {: wa:ptr wu:n :}
   DRV-NAME!
   s" : " DRV DRV-U DRV-CAP PUT
   DRV-NAME$ DRV+
   s"  ( -- ) " DRV+
   SB-RESET IN-N @ FMT:SB-U SB$ DRV+
   s"  0 ?do i JUDGE-FUZZ:IN@ " DRV+
   wa wu DRV+
   s"  i JUDGE-FUZZ:ANS! loop ;" DRV+
   DRV DRV-U @ DRV-NAME$ JUDGE-COST:DEFINE-RUN ;

: KEEP-ENGINE ( -- )
   IN-N @ 0 ?do  ANS i cells + @  ENG-ANS i cells + !  loop ;

\ ---- the inputs both columns are run on ---------------------------------------
\ The ends of the signed range first, where a lost sign or a narrowed width is
\ visible and nowhere else is, then whatever the generator says.

: IN! ( n n -- ) {: v:n k:n :}
   k IN-MAX >= if E-JUDGE-FUZZ-CAP throw then
   v IN-VALS k cells + ! ;

public

\ How many boundary inputs every program is run on, before the generated ones.
5 constant BOUNDARY-N

private

: BOUNDARY! ( -- )
   MIN-INT 0 IN!
   MAX-INT 1 IN!
   -1 2 IN!
   0 3 IN!
   1 4 IN! ;

: INPUTS! ( n -- ) {: randoms:n :}
   BOUNDARY-N randoms + IN-MAX > if E-JUDGE-FUZZ-CAP throw then
   BOUNDARY-N randoms + IN-N !
   BOUNDARY!
   randoms 0 ?do NEXT BOUNDARY-N i + IN! loop ;

\ ---- one comparison ------------------------------------------------------------

\ The two texts this comparison was handed. They are the same text in a sweep and
\ two different ones only when the comparison itself is under test, so both are
\ printed: a diagnostic that showed the generator's LAST program rather than the
\ one that disagreed would name the wrong body every time the two differ.
variable ENG-SRC
variable ENG-SRC-U
variable CHN-SRC
variable CHN-SRC-U

: SRCS! ( ptr u8 n ptr u8 n -- ) {: ea:ptr eu:n ca:ptr cu:n :}
   ea ENG-SRC 0 ptr-field !  eu ENG-SRC-U !
   ca CHN-SRC 0 ptr-field !  cu CHN-SRC-U ! ;

: ENG-SRC$ ( -- ptr u8 n )  ENG-SRC 0 ptr-field @ ENG-SRC-U @ ;
: CHN-SRC$ ( -- ptr u8 n )  CHN-SRC 0 ptr-field @ CHN-SRC-U @ ;

\ How many disagreements are printed before the rest are only counted. A broken
\ code generator disagrees about everything, and a report that printed all of it
\ would bury the first one, which is the one worth reading.
4 constant PRINT-MAX

: SAY-MISMATCH ( n n n -- ) {: k:n ev:n cv:n :}
   MISMATCH-CELL @ PRINT-MAX > if exit then
   s" judge-fuzz: the two code generators disagree" type cr
   s"   engine program: " type ENG-SRC$ type cr
   s"   chain program:  " type CHN-SRC$ type cr
   s"   input:          " type k IN@ FMT:.INT cr
   s"   engine answer:  " type ev FMT:.INT cr
   s"   chain answer:   " type cv FMT:.INT cr
   MISMATCH-CELL @ PRINT-MAX = if
      s" judge-fuzz: further disagreements are counted and not printed" type cr
   then ;

: COMPARE ( -- )
   IN-N @ 0 ?do
      ENG-ANS i cells + @ {: ev:n :}
      ANS i cells + @ {: cv:n :}
      CHECKS-CELL @ 1+ CHECKS-CELL !
      ev cv <> if
         MISMATCH-CELL @ 1+ MISMATCH-CELL !
         i ev cv SAY-MISMATCH
      then
   loop ;

public

\ Compile the FIRST text through the engine, the SECOND through the chain, and
\ hold their answers against each other on every input. The oracle is this word
\ with one text in both places; handing it two texts that differ is how the
\ comparison itself is tested, because an oracle that cannot see a difference
\ would pass every day and mean nothing.
: PAIR ( ptr u8 n ptr u8 n n -- ) {: ea:ptr eu:n ca:ptr cu:n randoms:n :}
   ea eu ca cu SRCS!
   randoms INPUTS!
   ea eu ENGINE-COMPILE {: entry-e:n :}
   ca cu CHAIN-COMPILE {: entry-c:n :}
   entry-e 0= entry-c 0= or if E-JUDGE-FUZZ-COLUMN throw then
   entry-e entry-c = if E-JUDGE-FUZZ-COLUMN throw then
   ENG-NAME$ DRIVE
   KEEP-ENGINE
   CHN-NAME$ DRIVE
   COMPARE ;

: BODIES ( -- n )      BODIES-CELL @ ;
: CHECKS ( -- n )      CHECKS-CELL @ ;
: MISMATCHES ( -- n )  MISMATCH-CELL @ ;

\ Start the generator from its constant seed and clear the counters.
: RESET ( -- )
   SEED STATE !
   0 BODIES-CELL !
   0 CHECKS-CELL !
   0 MISMATCH-CELL ! ;

\ One generated program through both code generators, on the boundary inputs and
\ this many generated ones.
: ROUND ( n -- ) {: randoms:n :}
   BODY$ {: a:ptr u:n :}
   a u a u randoms PAIR
   BODIES-CELL @ 1+ BODIES-CELL ! ;

\ A whole sweep: this many programs, each on the boundary inputs and this many
\ generated ones.
: RUN ( n n -- ) {: bodies:n randoms:n :}
   RESET
   bodies 0 ?do randoms ROUND loop ;

;package
