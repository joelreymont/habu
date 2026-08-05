\ codegen-compare-gap.f - the words a corpus holds that the native chain cannot
\ compile yet, and the check that every corpus word is accounted for.
\ One concern: the gap account of a measured pass.
\
\ EVERY CORPUS WORD IS ACCOUNTED FOR. A word is either compiled - the real chain
\ runs on it and it gets a row of its own - or declared a gap that names the
\ capability the chain still lacks. Nothing is skipped: COVERAGE-CK refuses a
\ pass in which some corpus word is neither, so "the new column has fewer rows"
\ can only ever mean "these named capabilities are missing", never "the harness
\ quietly stopped looking".
\
\ THERE IS NO WAY TO DECLARE A GAP WITHOUT A CAPABILITY. GAP takes one, by type,
\ and GAP-ALSO adds the next; a row that named none would have to be written past
\ this package's own declarers, and the decoder refuses a code outside the
\ vocabulary at first touch. That is what keeps a shorter new column from ever
\ reading as an excuse.
\
\ A GAP ROW NAMES EVERY CAPABILITY IT NEEDS rather than the first that stops it.
\ A word that needs a branch and a comparison is not unblocked by branches alone,
\ and a reader planning the next capability should see that.
\
\ THIS FILE IS SHARED BY ALL FOUR CORPORA AND HOLDS ONE STORE. A measurement
\ pass holds one corpus at a time - tools/codegen-compare-cases.f measures the
\ first, tools/codegen-compare-cases2.f the second,
\ tools/codegen-compare-cases3.f the third and tools/codegen-compare-cases4.f
\ the fourth, each opening with a RESET - so one store serves them all and the
\ report renders whichever pass just ran without knowing which corpus it was.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f

package CODEGEN-GAP

public

\ What a corpus word needs that the chain has not got. A gap row stores these
\ rather than a sentence, so a row that names no capability at all is unwritable
\ and the report renders every one of them the same way. In order: a branch, a
\ loop, or an exit from the middle of one; a typed locals frame; calling another
\ word, recursion included; a load or a store; an ordering or equality
\ operation; integer division; float arithmetic; placing a double anywhere but a
\ straight line.
\
\ WHAT `floats` COVERS, because one word has to stand for two refusals. A float
\ body is stopped twice over: the tape has no kind for a real literal
\ (E-NFEED-KIND, src/compiler/native/feed.f:174) and the dialect has no model
\ for a float operation (E-HIR-UNMODELED). Both are measured at the head of
\ tools/codegen-compare-new3.f. They are one capability here because neither is
\ any use without the other - a chain that read the literal and could not add,
\ or that could add and could not read the literal, compiles no float body at
\ all.
\
\ AND WHY `float-place` IS A CAPABILITY OF ITS OWN RATHER THAN PART OF `floats`.
\ The two are not two halves of one thing the way the literal and the operation
\ are: a chain that has `floats` compiles every straight-line float body there
\ is, and several of this corpus's rows are exactly that. What `float-place`
\ names is the SECOND question - where a double may live between operations -
\ and it is a different refusal in a different stage, E-NELAB-TYPE from
\ src/compiler/native/elaborate.f, reached only by a body that carries a double
\ across a block edge, across a call, or into a memory cell. Keeping them one
\ word would have made every gap row that waits for either read as waiting for
\ both, which is the opposite of what this account is for. Dots
\ habu-carry-a-double-570d2f5c and habu-store-a-double-a31b313e carry it.
\
\ AND WHY `loop-spill` IS A CAPABILITY AND NOT REGISTER PRESSURE IN GENERAL.
\ Register pressure on its own is no longer a refusal: the multi-block allocator
\ spills, and src/compiler/native/spill.f turns its decisions into real stores
\ and loads. What it will not spill is a class whose value is defined or read in
\ a block that is neither the one the routine is entered through nor the one
\ control leaves through - src/compiler/native/regalloc.f says so where
\ MB-SPILLABLE? decides it, because a frame access inside such a block would sit
\ where the memory order cannot be stated. The body of a loop is exactly such a
\ block, so a loop that holds more values at once than the machine has registers
\ is refused with E-A64RA-SPILL however large a register budget it is given. It
\ is a capability of its own because a chain that gained it would compile a class
\ of bodies nothing else unblocks, and because the refusal names a place rather
\ than a shortage: the same body outside a loop is allocated. Dot
\ habu-spill-from-a-4145325c carries it.
ENUM cap DERIVE eq
   control-flow
   locals
   calls
   memory
   comparison
   division
   floats
   float-place
   loop-spill
;ENUM

private

16 constant GAP-MAX

\ How many capabilities one gap row may name. It is the size of the vocabulary
\ above, so a row that genuinely waits for everything can say so.
9 constant CAP-MAX

GAP-MAX CODEGEN-COMPARE:NAME-MAX * BUFFER: GAP-NAMES
create GAP-LENS GAP-MAX cells allot
create GAP-CAP-N GAP-MAX cells allot
create GAP-CAPS GAP-MAX CAP-MAX * cells allot

variable GAP-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

: GAP-OK ( n -- n )
   dup 0 < over GAP-N @ >= or if E-CODEGEN-COMPARE-ROW throw then ;

: GAP-NAME-AT ( n -- ptr u8 )
   CODEGEN-COMPARE:NAME-MAX * GAP-NAMES + ;

\ A stored row is cells, so a capability crosses to a number here and back
\ there. The decoder is exhaustive and refuses a code outside the vocabulary at
\ first touch, so a corrupted row cannot decode as some other capability.
: CAP-CODE ( CODEGEN-GAP:cap -- n )
   MATCH cap
      control-flow OF 0 ENDOF
      locals       OF 1 ENDOF
      calls        OF 2 ENDOF
      memory       OF 3 ENDOF
      comparison   OF 4 ENDOF
      division     OF 5 ENDOF
      floats       OF 6 ENDOF
      float-place  OF 7 ENDOF
      loop-spill   OF 8 ENDOF
   ;MATCH ;

: N>CAP ( n -- CODEGEN-GAP:cap )
   case
      0 of CODEGEN--GAP-CAP:CONTROL-FLOW endof
      1 of CODEGEN--GAP-CAP:LOCALS endof
      2 of CODEGEN--GAP-CAP:CALLS endof
      3 of CODEGEN--GAP-CAP:MEMORY endof
      4 of CODEGEN--GAP-CAP:COMPARISON endof
      5 of CODEGEN--GAP-CAP:DIVISION endof
      6 of CODEGEN--GAP-CAP:FLOATS endof
      7 of CODEGEN--GAP-CAP:FLOAT-PLACE endof
      8 of CODEGEN--GAP-CAP:LOOP-SPILL endof
      E-CODEGEN-COMPARE-ROW throw
   endcase ;

\ Every name a gap writes down has to be a corpus word the old column really
\ measured. A misspelling would otherwise become a gap for a word that does not
\ exist, or leave a real word accounted for by nothing.
: CORPUS-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-OLD a u CODEGEN-COMPARE:FIND-ROW 0 < if
      E-CODEGEN-COMPARE-CORPUS throw
   then ;

public

\ Declare a corpus word the chain cannot express yet, and the first capability
\ it is waiting for. There is no way to declare one without a capability.
: GAP ( ptr u8 n CODEGEN-GAP:cap -- ) {: a:ptr u:n c:CODEGEN-GAP:cap :}
   GAP-N @ GAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   u CODEGEN-COMPARE:NAME-MAX > if E-CODEGEN-COMPARE-CAP throw then
   a u CORPUS-CK
   a  GAP-N @ GAP-NAME-AT  u STR-LEN BYTE-COPY-LEN
   u GAP-LENS GAP-N @ SLOT !
   c CAP-CODE GAP-CAPS GAP-N @ CAP-MAX * SLOT !
   1 GAP-CAP-N GAP-N @ SLOT !
   GAP-N @ 1+ GAP-N ! ;

\ Another capability the gap just declared is also waiting for.
: GAP-ALSO ( CODEGEN-GAP:cap -- ) {: c:CODEGEN-GAP:cap :}
   GAP-N @ 1- GAP-OK {: k:n :}
   GAP-CAP-N k SLOT @ {: j:n :}
   j CAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   c CAP-CODE GAP-CAPS k CAP-MAX * j + SLOT !
   j 1+ GAP-CAP-N k SLOT ! ;

: GAPS ( -- n )
   GAP-N @ ;

: GAP-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k GAP-OK GAP-NAME-AT
   GAP-LENS k SLOT @ ;

: GAP-CAPS@ ( n -- n ) {: k:n :}
   GAP-CAP-N k GAP-OK SLOT @ ;

: GAP-CAP@ ( n n -- CODEGEN-GAP:cap ) {: k:n j:n :}
   k GAP-OK drop
   j 0 < j k GAP-CAPS@ >= or if E-CODEGEN-COMPARE-ROW throw then
   GAP-CAPS k CAP-MAX * j + SLOT @ N>CAP ;

\ How a capability reads in the report. The one place a capability becomes text.
: CAP$ ( CODEGEN-GAP:cap -- ptr u8 n ) {: c:CODEGEN-GAP:cap :}
   c MATCH cap
      control-flow OF s" control flow" ENDOF
      locals       OF s" locals" ENDOF
      calls        OF s" calls" ENDOF
      memory       OF s" memory access" ENDOF
      comparison   OF s" comparison" ENDOF
      division     OF s" division" ENDOF
      floats       OF s" floats" ENDOF
      float-place  OF s" placing a double" ENDOF
      loop-spill   OF s" spilling inside a loop" ENDOF
   ;MATCH ;

: RESET ( -- )
   0 GAP-N ! ;

private

: GAP-FOR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   false
   GAP-N @ 0 ?do
      i GAP-NAME$ a u STR= if drop true leave then
   loop ;

public

\ Every corpus word is compiled or declared a gap. A word that is neither would
\ leave the new column quietly shorter than the old one, which is the one
\ failure a comparison harness must never have. Called at the end of a new
\ column's pass, over the rows that pass left behind.
: COVERAGE-CK ( -- )
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         CODEGEN-COMPARE:PATH-NEW i CODEGEN-COMPARE:NAME$
         CODEGEN-COMPARE:FIND-ROW 0 < if
            i CODEGEN-COMPARE:NAME$ GAP-FOR? 0= if
               E-CODEGEN-COMPARE-CORPUS throw
            then
         then
      then
   loop ;

\ ONE NEW COLUMN'S WHOLE ACCOUNT: the gap list cleared, the caller's compiled
\ rows and declared gaps, and the check that between them they cover every
\ corpus word. Every corpus needs those three in that order, so the order is
\ here once and each new column hands in the part that is its own. The check is
\ what makes a short new column read as named missing capabilities rather than
\ as a harness that stopped looking, and it cannot be forgotten by a column
\ that reaches its account through this word.
\ typed-local-lint: allow-bare-local - declare is the column's own body, and a
\ local annotation cannot carry a quotation effect.
: ACCOUNT ( [ -- ] -- ) {: declare :}
   RESET
   declare execute
   COVERAGE-CK ;

;package
