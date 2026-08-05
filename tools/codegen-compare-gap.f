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
\ THERE IS NO WAY TO DECLARE A GAP WITHOUT A CAPABILITY. GAP takes one, by
\ type; a row that named none would have to be written past this package's own
\ declarer. That is what keeps a shorter new column from ever reading as an
\ excuse.
\
\ ONE CAPABILITY REMAINS. The vocabulary once named nine classes; the chain
\ closed eight of them (control flow, locals, calls, memory, comparison,
\ division, floats, placing a double) and their variants were deleted with the
\ campaign that closed them (dot habu-collapse-the-comparison-b7ada325). What
\ is left is `loop-spill`, the one class with live gap rows.
\
\ WHY `loop-spill` IS A CAPABILITY AND NOT REGISTER PRESSURE IN GENERAL.
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

ENUM cap DERIVE eq
   loop-spill
;ENUM

private

16 constant GAP-MAX

GAP-MAX CODEGEN-COMPARE:NAME-MAX * BUFFER: GAP-NAMES
create GAP-LENS GAP-MAX cells allot

variable GAP-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

: GAP-OK ( n -- n )
   dup 0 < over GAP-N @ >= or if E-CODEGEN-COMPARE-ROW throw then ;

: GAP-NAME-AT ( n -- ptr u8 )
   CODEGEN-COMPARE:NAME-MAX * GAP-NAMES + ;

\ Every name a gap writes down has to be a corpus word the old column really
\ measured. A misspelling would otherwise become a gap for a word that does not
\ exist, or leave a real word accounted for by nothing.
: CORPUS-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-OLD a u CODEGEN-COMPARE:FIND-ROW 0 < if
      E-CODEGEN-COMPARE-CORPUS throw
   then ;

public

\ Declare a corpus word the chain cannot express yet, and the capability it is
\ waiting for. There is no way to declare one without a capability, and the
\ vocabulary holds one, so every row names exactly that one.
: GAP ( ptr u8 n CODEGEN-GAP:cap -- ) {: a:ptr u:n c:CODEGEN-GAP:cap :}
   GAP-N @ GAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   u CODEGEN-COMPARE:NAME-MAX > if E-CODEGEN-COMPARE-CAP throw then
   a u CORPUS-CK
   a  GAP-N @ GAP-NAME-AT  u STR-LEN BYTE-COPY-LEN
   u GAP-LENS GAP-N @ SLOT !
   GAP-N @ 1+ GAP-N ! ;

: GAPS ( -- n )
   GAP-N @ ;

: GAP-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k GAP-OK GAP-NAME-AT
   GAP-LENS k SLOT @ ;

\ Every row names exactly one capability: GAP's own signature guarantees it,
\ so the count is a fact about the declarer, not a stored cell.
: GAP-CAPS@ ( n -- n )
   GAP-OK drop 1 ;

: GAP-CAP@ ( n n -- CODEGEN-GAP:cap ) {: k:n j:n :}
   k GAP-OK drop
   j 0 <> if E-CODEGEN-COMPARE-ROW throw then
   CODEGEN--GAP-CAP:LOOP-SPILL ;

\ How a capability reads in the report. The one place a capability becomes text.
: CAP$ ( CODEGEN-GAP:cap -- ptr u8 n ) {: c:CODEGEN-GAP:cap :}
   c MATCH cap
      loop-spill OF s" spilling inside a loop" ENDOF
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
