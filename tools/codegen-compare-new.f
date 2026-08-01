\ codegen-compare-new.f - the new code generator's column of the comparison.
\ One concern: which corpus word the new chain can compile, and what it costs.
\
\ EVERY CORPUS WORD IS ACCOUNTED FOR. A word is either compiled - the real chain
\ runs on it and it gets a row of its own - or declared a gap that names the
\ capability the straight-line subset still lacks. Nothing is skipped: the
\ coverage check below refuses a pass in which some corpus word is neither, so
\ "the new column has fewer rows" can only ever mean "these named capabilities
\ are missing", never "the harness quietly stopped looking".
\
\ THE GAP LIST IS EMPTY. All eleven corpus words are compiled by the chain, which
\ is what makes the table a comparison of two code generators over one corpus
\ rather than over the part of it one of them can express. The gap vocabulary
\ below stays, because the next capability the chain lacks has to be nameable.
\
\ WHAT THE SUBSET IS TODAY. src/compiler/native/hir-word.f declares thirty-two
\ source words - `+ - * / < <= =`, `1-` and `1+`, the four memory words `@`, `!`,
\ `c@` and `c!`, the ten control words `if then begin until ?do loop i unloop
\ exit RECURSE`, the two halves `{:` and `:}` of a typed locals group, and the
\ seven renames `2dup dup drop swap over nip rot` - plus integer literals, and
\ src/compiler/native/hir.f gives the dialect seventeen operations. A word of the
\ corpus is expressible exactly when its body is those words and nothing else,
\ plus - for a body that names a `create`d data word - the one address the
\ harness states (dot habu-resolve-a-data-a1c8067f). All eleven are: the empty
\ word, the three-argument sum, the sum of two squares - which is the one that
\ shows the renames costing nothing at all - the two-way branch, the typed
\ locals frame, both loop forms, the recursion, the cell bump, the byte sum and
\ the byte scan.
\
\ WHAT A GAP ROW WOULD SAY. There is none today, and when there is one again its
\ row names every capability it needs rather than the first that stops it - a
\ word that needs a branch and a comparison is not unblocked by branches alone,
\ and a reader planning the next capability should see that.
\
\ HOW A COVERED ROW IS CHECKED. The word the chain compiled is CALLED on the same
\ pinned inputs the old column used, and its answers are recorded as that row's
\ outputs. The head-to-head check is then an exact comparison of the two rows'
\ outputs: the same corpus word compiled two ways has to compute the same thing,
\ and a row where it does not is a finding the run reports and exits non-zero on.
\ Bytes are exact too, and both columns' byte counts now come off a dictionary
\ record rather than one off a record and one off an emitter's report.
\
\ AND IT IS CALLED THE WAY EVERY WORD IS. The new chain's routines are published
\ as ordinary dictionary words by tools/codegen-compare-migrated.f before this
\ file is compiled, so a body below names a word and the engine resolves that
\ call exactly as it resolves the old column's - a direct branch to the callee, or
\ the callee's body copied into the caller when it is small enough for that.
\ Nothing here pushes an address, and nothing here executes one. The paragraph
\ that used to stand at this point, telling a reader that the new column paid one
\ indirect branch the old one did not, is gone because the difference is gone:
\ the two calibration rows measure the same kind of call, the report prints both,
\ and the check reports it as a finding if they drift apart.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-migrated.f

package CODEGEN-NEW

public

\ What a corpus word needs that the straight-line subset has not got. A gap row
\ stores these rather than a sentence, so a row that names no capability at all
\ is unwritable and the report renders every one of them the same way. In order:
\ a branch, a loop, or an exit from the middle of one; a typed locals frame;
\ calling another word, recursion included; a load or a store; an ordering or
\ equality operation; integer division.
ENUM cap DERIVE eq
   control-flow
   locals
   calls
   memory
   comparison
   division
;ENUM

private

16 constant GAP-MAX
6 constant CAP-MAX

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
: CAP-CODE ( CODEGEN-NEW:cap -- n )
   MATCH cap
      control-flow OF 0 ENDOF
      locals       OF 1 ENDOF
      calls        OF 2 ENDOF
      memory       OF 3 ENDOF
      comparison   OF 4 ENDOF
      division     OF 5 ENDOF
   ;MATCH ;

: N>CAP ( n -- CODEGEN-NEW:cap )
   case
      0 of CODEGEN--NEW-CAP:CONTROL-FLOW endof
      1 of CODEGEN--NEW-CAP:LOCALS endof
      2 of CODEGEN--NEW-CAP:CALLS endof
      3 of CODEGEN--NEW-CAP:MEMORY endof
      4 of CODEGEN--NEW-CAP:COMPARISON endof
      5 of CODEGEN--NEW-CAP:DIVISION endof
      E-CODEGEN-COMPARE-ROW throw
   endcase ;

\ Every name this file writes down has to be a corpus word the old column really
\ measured. A misspelling would otherwise become a gap for a word that does not
\ exist, or leave a real word accounted for by nothing.
: CORPUS-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-OLD a u CODEGEN-COMPARE:FIND-ROW 0 < if
      E-CODEGEN-COMPARE-CORPUS throw
   then ;

public

\ Declare a corpus word the straight-line subset cannot express yet, and the
\ first capability it is waiting for. There is no way to declare one without a
\ capability.
: GAP ( ptr u8 n CODEGEN-NEW:cap -- ) {: a:ptr u:n c:CODEGEN-NEW:cap :}
   GAP-N @ GAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   u CODEGEN-COMPARE:NAME-MAX > if E-CODEGEN-COMPARE-CAP throw then
   a u CORPUS-CK
   a  GAP-N @ GAP-NAME-AT  u STR-LEN BYTE-COPY-LEN
   u GAP-LENS GAP-N @ SLOT !
   c CAP-CODE GAP-CAPS GAP-N @ CAP-MAX * SLOT !
   1 GAP-CAP-N GAP-N @ SLOT !
   GAP-N @ 1+ GAP-N ! ;

\ Another capability the gap just declared is also waiting for.
: GAP-ALSO ( CODEGEN-NEW:cap -- ) {: c:CODEGEN-NEW:cap :}
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

: GAP-CAP@ ( n n -- CODEGEN-NEW:cap ) {: k:n j:n :}
   k GAP-OK drop
   j 0 < j k GAP-CAPS@ >= or if E-CODEGEN-COMPARE-ROW throw then
   GAP-CAPS k CAP-MAX * j + SLOT @ N>CAP ;

\ How a capability reads in the report. The one place a capability becomes text.
: CAP$ ( CODEGEN-NEW:cap -- ptr u8 n ) {: c:CODEGEN-NEW:cap :}
   c MATCH cap
      control-flow OF s" control flow" ENDOF
      locals       OF s" locals" ENDOF
      calls        OF s" calls" ENDOF
      memory       OF s" memory access" ENDOF
      comparison   OF s" comparison" ENDOF
      division     OF s" division" ENDOF
   ;MATCH ;

private

\ ---- the covered words -------------------------------------------------------
\ Each row names the corpus word it is compared against, the word that carries
\ the new chain's code for the same body, and the pinned inputs the old column
\ used. The bodies themselves are in tools/codegen-compare-migrated.f, which
\ compiled and published them before this file was compiled - which is why a
\ call below is an ordinary call to an ordinary word and there is no chain,
\ context or published address anywhere in it.
\
\ NOTHING HERE CATCHES. A word this file names that the migration did not
\ publish is a claim the comparison made and did not keep, and it must surface as
\ the missing subject rather than as a row that quietly went away.

\ `: NOOP ( -- ) ;` - the calibration row. It returns nothing, so it has no
\ output to compare; what it measures is the floor of a call on this path, which
\ every other new row is divided by. Both paths' floors are now the same call
\ into the same kind of record, so the two are expected to agree and the report
\ prints them side by side.
: NOOP-CASE ( -- )
   s" CODEGEN-CORPUS:NOOP" s" CODEGEN-CORPUS:NOOP-N"
   [: CODEGEN-CORPUS:NOOP-N ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE-NEW
   CODEGEN-COMPARE:CALIBRATE ;

: ADD3-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3" s" CODEGEN-CORPUS:ADD3-N"
   [: 1 2 3 CODEGEN-CORPUS:ADD3-N drop ;]
   [: 1 2 3 CODEGEN-CORPUS:ADD3-N CODEGEN-COMPARE:VECTOR
      -5 5 7 CODEGEN-CORPUS:ADD3-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ Four renames and three operations, and the renames are where the new chain
\ stops paying.
: SQUARE-SUM-CASE ( -- )
   s" CODEGEN-CORPUS:SQUARE-SUM" s" CODEGEN-CORPUS:SQUARE-SUM-N"
   [: 3 4 CODEGEN-CORPUS:SQUARE-SUM-N drop ;]
   [: 3 4 CODEGEN-CORPUS:SQUARE-SUM-N CODEGEN-COMPARE:VECTOR
      -2 5 CODEGEN-CORPUS:SQUARE-SUM-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The two-way branch. It is the smallest word in the corpus whose answer depends
\ on which way a branch went, so the head-to-head check below - both argument
\ orders, the same two the old column uses - is what says the branch went the
\ right way.
: MAX2-CASE ( -- )
   s" CODEGEN-CORPUS:MAX2" s" CODEGEN-CORPUS:MAX2-N"
   [: 3 4 CODEGEN-CORPUS:MAX2-N drop ;]
   [: 3 4 CODEGEN-CORPUS:MAX2-N CODEGEN-COMPARE:VECTOR
      9 -1 CODEGEN-CORPUS:MAX2-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The typed locals frame, and the one word of the corpus that divides.
\
\ WHAT MAKES THE TWO PINNED INPUTS A CHECK ON THE BINDING ORDER. `{: a b t :}`
\ over a stack holding a, b, t must bind a to the DEEPEST value; binding it to
\ the top instead is the one mistake a locals frame can make. The first pinned
\ input cannot see it - (10, 20, 50) answers 15 either way, because the
\ subtraction is symmetric about the midpoint there - and the second one can:
\ (0, 100, 25) answers 25 with the right binding and 75 with a and b swapped, so
\ the head-to-head check against the old column catches it.
: LERP-CASE ( -- )
   s" CODEGEN-CORPUS:LERP" s" CODEGEN-CORPUS:LERP-N"
   [: 10 20 50 CODEGEN-CORPUS:LERP-N drop ;]
   [: 10 20 50 CODEGEN-CORPUS:LERP-N CODEGEN-COMPARE:VECTOR
      0 100 25 CODEGEN-CORPUS:LERP-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The counted loop, and the one place in the corpus where the loop index is a
\ value the chain carries in a register rather than a frame the engine pushes.
: SUM-TO-CASE ( -- )
   s" CODEGEN-CORPUS:SUM-TO" s" CODEGEN-CORPUS:SUM-TO-N"
   [: 16 CODEGEN-CORPUS:SUM-TO-N drop ;]
   [: 16 CODEGEN-CORPUS:SUM-TO-N CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS:SUM-TO-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The other loop form, with the test at the end. The second pinned input is
\ negative, so the loop runs once and leaves; the first counts all the way down.
\ Between them they measure both ways out of a back edge.
: COUNT-DOWN-CASE ( -- )
   s" CODEGEN-CORPUS:COUNT-DOWN" s" CODEGEN-CORPUS:COUNT-DOWN-N"
   [: 16 CODEGEN-CORPUS:COUNT-DOWN-N drop ;]
   [: 16 CODEGEN-CORPUS:COUNT-DOWN-N CODEGEN-COMPARE:VECTOR
      -3 CODEGEN-CORPUS:COUNT-DOWN-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The pinned inputs of the two byte-span words, written here the way every other
\ covered body writes its pinned inputs: as the literal the old column uses. The
\ subject text is the one tools/codegen-compare-cases.f measures the old words
\ on, so the two columns scan the same bytes.
: SUBJECT$ ( -- ptr u8 n )
   s" habu codegen baseline" ;

: EMPTY$ ( -- ptr u8 n )
   s" " ;

\ The memory word. Both columns bump the SAME cell - the migrated body names the
\ corpus's own private cell - and both record its contents as an output. That is
\ what makes the head-to-head check a statement about the store and the load: a
\ routine that computed `n 1+` and touched no memory would answer the same two
\ numbers and fail on the other two.
: CELL-BUMP-CASE ( -- )
   s" CODEGEN-CORPUS:CELL-BUMP" s" CODEGEN-CORPUS:CELL-BUMP-N"
   [: 7 CODEGEN-CORPUS:CELL-BUMP-N drop ;]
   [: 7 CODEGEN-CORPUS:CELL-BUMP-N CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS:BUMP-CELL@ CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS:CELL-BUMP-N CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS:BUMP-CELL@ CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ Byte-width memory inside a loop, which is the first body of the corpus that
\ needs the memory order to cross an edge: the load is in the loop body, so the
\ order the second turn reads is the one the first turn left, and it reaches the
\ body as a block argument. The two pinned inputs are the subject text and an
\ EMPTY span, so the zero-trip path out of a `?do` is measured as well as the
\ counting one - and on the empty span the loop body never runs, which is the
\ case a routine that ordered its accesses wrongly could still get right.
: BYTE-SUM-CASE ( -- )
   s" CODEGEN-CORPUS:BYTE-SUM" s" CODEGEN-CORPUS:BYTE-SUM-N"
   [: SUBJECT$ CODEGEN-CORPUS:BYTE-SUM-N drop ;]
   [: SUBJECT$ CODEGEN-CORPUS:BYTE-SUM-N CODEGEN-COMPARE:VECTOR
      EMPTY$ CODEGEN-CORPUS:BYTE-SUM-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The recursion, which is also the plain word-call-and-return shape. It is the
\ one corpus word whose routine is not a leaf: it reserves a frame and puts its
\ caller's return address in it, because the first call would otherwise destroy
\ it, and every value it still needs crosses the call on its own data stack,
\ because no register of the caller survives a call to a routine whose contract
\ destroys the whole pool. The two pinned inputs are the two ways through - ten
\ recurses ten deep, one takes the base-case arm and never calls at all.
: FACT-CASE ( -- )
   s" CODEGEN-CORPUS:FACT" s" CODEGEN-CORPUS:FACT-N"
   [: 10 CODEGEN-CORPUS:FACT-N drop ;]
   [: 10 CODEGEN-CORPUS:FACT-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS:FACT-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The byte scan that leaves from the middle of its loop. Three capabilities meet
\ in it: the byte load, the memory order crossing the loop edge, and `exit`,
\ which branches to the block the return is in rather than returning a second
\ time. The two pinned inputs are a byte that IS in the subject text and one that
\ is not, so both ways out are measured - the early one and the one that runs the
\ loop to its end and answers the miss.
103 constant LETTER-G                \ present in the subject text
122 constant LETTER-Z                \ absent from it

: BYTE-FIND-CASE ( -- )
   s" CODEGEN-CORPUS:BYTE-FIND" s" CODEGEN-CORPUS:BYTE-FIND-N"
   [: SUBJECT$ LETTER-G CODEGEN-CORPUS:BYTE-FIND-N drop ;]
   [: SUBJECT$ LETTER-G CODEGEN-CORPUS:BYTE-FIND-N CODEGEN-COMPARE:VECTOR
      SUBJECT$ LETTER-Z CODEGEN-CORPUS:BYTE-FIND-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: COVERED-CASES ( -- )
   NOOP-CASE
   ADD3-CASE
   SQUARE-SUM-CASE
   MAX2-CASE
   LERP-CASE
   SUM-TO-CASE
   COUNT-DOWN-CASE
   CELL-BUMP-CASE
   BYTE-SUM-CASE
   BYTE-FIND-CASE
   FACT-CASE ;

\ ---- the words the subset cannot express yet ---------------------------------
\ None. Every word of the corpus is compiled by the chain, so the gap list is
\ empty and COVERAGE-CK below is what says so - it refuses a pass in which any
\ corpus word is neither compiled nor declared a gap, so an empty list is a
\ statement about all eleven rather than the absence of one.
\
\ THE MACHINERY STAYS. A gap row is how the next capability that is missing gets
\ named, and deleting the vocabulary would mean the next word this chain cannot
\ express is a shorter table instead of a named capability.
: GAP-CASES ( -- ) ;

\ ---- accounting --------------------------------------------------------------
: GAP-FOR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   false
   GAP-N @ 0 ?do
      i GAP-NAME$ a u STR= if drop true leave then
   loop ;

\ Every corpus word is compiled or declared a gap. A word that is neither would
\ leave the new column quietly shorter than the old one, which is the one failure
\ a comparison harness must never have.
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

public

: RESET ( -- )
   0 GAP-N ! ;

\ Compile every corpus word the subset can express, declare the rest, and check
\ that between them they account for all of it. Runs after the old column, whose
\ rows the names are checked against.
: RUN ( -- )
   RESET
   COVERED-CASES
   GAP-CASES
   COVERAGE-CK ;

\ The old row this new row is the head-to-head partner of.
: PARTNER ( n -- n ) {: k:n :}
   CODEGEN-COMPARE:PATH-OLD k CODEGEN-COMPARE:NAME$ CODEGEN-COMPARE:FIND-ROW ;

\ Did the routine the new chain emitted compute what the old word computes? This
\ is the equality the whole comparison turns on.
: ROW-MATCH? ( n -- bool ) {: k:n :}
   k PARTNER {: b:n :}
   b 0 < if false exit then
   k b CODEGEN-COMPARE:SAME-OUTPUTS? ;

: MISMATCHES ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if
         i ROW-MATCH? 0= if 1+ then
      then
   loop ;

;package
