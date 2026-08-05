\ codegen-compare-new.f - the new code generator's column of the FIRST
\ comparison. One concern: which word of the pinned eleven the new chain
\ compiles, and what it costs.
\
\ EVERY CORPUS WORD IS ACCOUNTED FOR. A word is either compiled - the real chain
\ runs on it and it gets a row of its own - or declared a gap that names the
\ capability the chain still lacks. The account itself lives in
\ tools/codegen-compare-gap.f, which is shared with the second corpus's column;
\ this file only says which of these eleven words is which.
\
\ THE GAP LIST IS EMPTY. All eleven corpus words are compiled by the chain, which
\ is what makes the table a comparison of two code generators over one corpus
\ rather than over the part of it one of them can express.
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
require tools/codegen-compare-gap.f
require tools/codegen-compare-calibrate.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-migrated.f

package CODEGEN-NEW

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

\ The calibration row of this pass is CODEGEN-CALIBRATE:NEW, which every corpus
\ shares. Both paths' floors are the same call into the same kind of record, so
\ the two are expected to agree and the report prints them side by side.
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
   CODEGEN-CALIBRATE:NEW
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
\ None. Every word of this corpus is compiled by the chain, so the gap list is
\ empty and CODEGEN-GAP:COVERAGE-CK is what says so - it refuses a pass in which
\ any corpus word is neither compiled nor declared a gap, so an empty list is a
\ statement about all eleven rather than the absence of one.
: GAP-CASES ( -- ) ;

public

\ Compile every corpus word the subset can express, declare the rest, and check
\ that between them they account for all of it. Runs after the old column, whose
\ rows the names are checked against.
: RUN ( -- )
   [: COVERED-CASES GAP-CASES ;] CODEGEN-GAP:ACCOUNT ;

;package
