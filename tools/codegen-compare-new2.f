\ codegen-compare-new2.f - the new code generator's column of the SECOND
\ comparison. One concern: which word of the second corpus the new chain
\ compiles, and what it costs.
\
\ SIX ROWS AND ONE GAP. Six of the seven surveyed words are compiled by the
\ chain and measured beside the old emitter's code for the same body; one is
\ declared a gap that names the capabilities it is waiting for and the dot that
\ lands them. The account is tools/codegen-compare-gap.f's, shared with the first
\ corpus, and its coverage check refuses a pass in which any corpus word is
\ neither compiled nor declared - so six plus one is a statement about all seven.
\
\ TWO OF THE SIX ARE COMPILED FROM A RESPELLED CONSTANT, AND THE SUBSTITUTIONS
\ ARE LISTED IN FULL at the head of tools/codegen-compare-migrated2.f with the
\ dots that remove them: WS? writes 32, 9, 10 and 13 where the corpus names SP,
\ TAB, LF and CR, and SYM-FOLD-C writes 65, 90 and 32 where the corpus writes
\ $41, $5A and $20. The engine compiles each body and its twin to byte-identical
\ code, which tools/codegen-compare-test.f pins, so both columns are compiled
\ from the same program - but a reader of this table should know which two rows
\ carry a substitution and why, and that is what this paragraph is for.
\
\ WHAT THE GAP IS, IN FULL, BECAUSE A GAP IS A RESULT.
\
\   CODEGEN-CORPUS2:VEC-COPY-CELLS - calls, control flow.
\   It is worse than "not yet": the chain ACCEPTS a
\   `?do` body containing a call and emits a routine that computes the wrong
\   answer. Measured on this tree, with a chain-compiled callee:
\
\     : DBL-N ( n -- n ) dup + ;
\     : LC-N  ( n -- n ) 0 swap 0 ?do i DBL-N + loop ;
\     4 LC-N   answers 0; the engine's own code for the same body answers 12.
\
\   and with a body that stores through the addresses the callee returns - which
\   is what VEC-COPY-CELLS does - the routine faults. The same call in a
\   `begin … until` body is right, the same call in a `begin … while` TEST is
\   right (T-RES-WALK below measures exactly that), and the same `?do` body
\   calling an ENGINE-compiled word is right. So what is broken is a chain
\   routine's call from inside a counted loop: the loop's carried state lives in
\   registers of a pool that starts at the same register the callee's pool
\   starts at, and the site does not save it across a call to a routine that
\   declares it destroys them. Dot habu-save-the-loop-5f07e0c3.
\
\   THE ENGINE-COMPILED CALLEE IS NOT USED TO MAKE THIS ROW GREEN. It would
\   compile and it would answer correctly, and it would be a measurement resting
\   on a lucky fact - that the engine's own code for CELL-FIELD happens not to
\   touch the registers the loop is carrying - rather than on anything the two
\   routines' contracts promise each other. A row bought that way would go red
\   the first time the engine's emitter changed a register, for no reason a
\   reader could see. So the row is a gap, and it says why.
\
\ HOW A COVERED ROW IS CHECKED. The word the chain compiled is CALLED on the
\ same pinned inputs the old column used, and its answers are recorded as that
\ row's outputs. The head-to-head check is then an exact comparison of the two
\ rows' outputs: the same corpus word compiled two ways has to compute the same
\ thing, and a row where it does not is a finding the run reports and exits
\ non-zero on. Bytes are exact too, and both columns' byte counts come off a
\ dictionary record.
\
\ AND IT IS CALLED THE WAY EVERY WORD IS. The new chain's routines are published
\ as ordinary dictionary words by tools/codegen-compare-migrated2.f before this
\ file is compiled, so a body below names a word and the engine resolves that
\ call exactly as it resolves the old column's. Nothing here pushes an address,
\ and nothing here executes one.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-gap.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-corpus2.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated2.f

package CODEGEN-NEW2

private

\ ---- the covered words -------------------------------------------------------
\ Each row names the corpus word it is compared against, the word that carries
\ the new chain's code for the same body, and the pinned inputs the old column
\ used. The bodies themselves are in tools/codegen-compare-migrated2.f, which
\ compiled and published them before this file was compiled.
\
\ NOTHING HERE CATCHES. A word this file names that the migration did not
\ publish is a claim the comparison made and did not keep, and it must surface as
\ the missing subject rather than as a row that quietly went away.

\ The calibration row, which is the first corpus's empty call measured again in
\ this pass - the reason is in tools/codegen-compare-cases2.f beside the old
\ column's calibration case. It returns nothing, so it has no output to compare;
\ what it measures is the floor of a call on this path, which every other new
\ row is divided by.
: NOOP-CASE ( -- )
   s" CODEGEN-CORPUS:NOOP" s" CODEGEN-CORPUS:NOOP-N"
   [: CODEGEN-CORPUS:NOOP-N ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE-NEW
   CODEGEN-COMPARE:CALIBRATE ;

\ The pinned inputs, written here the way the first corpus's new column writes
\ its own: as the literals the old column uses, so the two columns are handed
\ the same numbers and neither reads the other's.
: SUBJECT$ ( -- ptr u8 n )
   s" aha aha aha" ;

: EMPTY$ ( -- ptr u8 n )
   s" " ;

97 constant LETTER-A
122 constant LETTER-Z

64 constant BELOW-A               \ $40, one under SYM-FOLD-C's lower bound
65 constant EXACTLY-A             \ $41, the lower bound itself
90 constant EXACTLY-Z             \ $5A, the upper bound itself
91 constant ABOVE-Z               \ $5B, one over the upper bound
97 constant ALREADY-LOWER

32 constant WS-SPACE
9 constant WS-TAB
10 constant WS-LF
13 constant WS-CR
97 constant NOT-WS

\ One mask. It is the smallest word of either corpus that does any work at all,
\ so it is where the entry cost and the body cost are closest together.
: TAG-CASE ( -- )
   s" CODEGEN-CORPUS2:TAG" s" CODEGEN-CORPUS2:TAG-N"
   [: 9 CODEGEN-CORPUS2:TAG-N drop ;]
   [: 9 CODEGEN-CORPUS2:TAG-N CODEGEN-COMPARE:VECTOR
      24 CODEGEN-CORPUS2:TAG-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS2:TAG-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ Four equalities folded with three `or`s, and the first row of either corpus
\ that is nothing but the comparison and bitwise vocabulary. One pinned input
\ per arm plus one that is no arm, so an arm that went missing is a different
\ answer rather than the same one.
: WS-CASE ( -- )
   s" CODEGEN-CORPUS2:WS?" s" CODEGEN-CORPUS2:WS?-N"
   [: WS-SPACE CODEGEN-CORPUS2:WS?-N drop ;]
   [: WS-SPACE CODEGEN-CORPUS2:WS?-N CODEGEN-COMPARE:VECTOR-FLAG
      WS-TAB CODEGEN-CORPUS2:WS?-N CODEGEN-COMPARE:VECTOR-FLAG
      WS-LF CODEGEN-CORPUS2:WS?-N CODEGEN-COMPARE:VECTOR-FLAG
      WS-CR CODEGEN-CORPUS2:WS?-N CODEGEN-COMPARE:VECTOR-FLAG
      NOT-WS CODEGEN-CORPUS2:WS?-N CODEGEN-COMPARE:VECTOR-FLAG ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ Two range tests, each leaving the word from the middle through `exit`, then
\ one `or`. Both bounds are pinned from both sides, which is what a `<` written
\ as a `<=` moves. This is one of the two rows compiled from a respelled
\ constant - see the head of this file.
: SYM-FOLD-CASE ( -- )
   s" CODEGEN-CORPUS2:SYM-FOLD-C" s" CODEGEN-CORPUS2:SYM-FOLD-C-N"
   [: EXACTLY-A CODEGEN-CORPUS2:SYM-FOLD-C-N drop ;]
   [: BELOW-A CODEGEN-CORPUS2:SYM-FOLD-C-N CODEGEN-COMPARE:VECTOR
      EXACTLY-A CODEGEN-CORPUS2:SYM-FOLD-C-N CODEGEN-COMPARE:VECTOR
      EXACTLY-Z CODEGEN-CORPUS2:SYM-FOLD-C-N CODEGEN-COMPARE:VECTOR
      ABOVE-Z CODEGEN-CORPUS2:SYM-FOLD-C-N CODEGEN-COMPARE:VECTOR
      ALREADY-LOWER CODEGEN-CORPUS2:SYM-FOLD-C-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The two-armed branch written with `else`, which the first corpus has no
\ example of: both arms produce the answer, so neither is a fall-through. Both
\ argument orders are pinned, because a branch taken the wrong way answers the
\ other argument and one order alone cannot see it.
: MAX-DIM-CASE ( -- )
   s" CODEGEN-CORPUS2:MAX-DIM" s" CODEGEN-CORPUS2:MAX-DIM-N"
   [: 3 7 CODEGEN-CORPUS2:MAX-DIM-N drop ;]
   [: 3 7 CODEGEN-CORPUS2:MAX-DIM-N CODEGEN-COMPARE:VECTOR
      7 3 CODEGEN-CORPUS2:MAX-DIM-N CODEGEN-COMPARE:VECTOR
      5 5 CODEGEN-CORPUS2:MAX-DIM-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ A `begin … while … repeat` scan carrying TWO values round the loop - a counter
\ and a cursor - with a byte load and a branch between them. It is the row that
\ needed more registers than any other: the chain refuses it at nine and accepts
\ it at ten.
: COUNT-CHAR-CASE ( -- )
   s" CODEGEN-CORPUS2:COUNT-CHAR" s" CODEGEN-CORPUS2:COUNT-CHAR-N"
   [: SUBJECT$ LETTER-A CODEGEN-CORPUS2:COUNT-CHAR-N drop ;]
   [: SUBJECT$ LETTER-A CODEGEN-CORPUS2:COUNT-CHAR-N CODEGEN-COMPARE:VECTOR
      SUBJECT$ LETTER-Z CODEGEN-CORPUS2:COUNT-CHAR-N CODEGEN-COMPARE:VECTOR
      EMPTY$ LETTER-A CODEGEN-CORPUS2:COUNT-CHAR-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ THE ROW THIS CORPUS EXISTS FOR: a loop whose TEST is a call. Every turn of the
\ loop crosses a call boundary, the value the loop carries is the callee's
\ answer, and both halves are the chain's code - TV-NEXT?-N is migrated before
\ T-RES-WALK-N and the caller branches to its record. The two columns each run
\ their own two-word program over the SAME binding table, so the check is about
\ the loads as well as the branches. Three pinned inputs, one per way out of the
\ callee: a chain of two links, a term that is not a variable, and a variable
\ bound to nothing.
: T-RES-WALK-CASE ( -- )
   s" CODEGEN-CORPUS2:T-RES-WALK" s" CODEGEN-CORPUS2:T-RES-WALK-N"
   [: CODEGEN-CORPUS2:CHAIN-HEAD CODEGEN-CORPUS2:T-RES-WALK-N drop ;]
   [: CODEGEN-CORPUS2:CHAIN-HEAD CODEGEN-CORPUS2:T-RES-WALK-N CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS2:NOT-A-VAR CODEGEN-CORPUS2:T-RES-WALK-N CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS2:UNBOUND-VAR CODEGEN-CORPUS2:T-RES-WALK-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: COVERED-CASES ( -- )
   NOOP-CASE
   TAG-CASE
   WS-CASE
   SYM-FOLD-CASE
   MAX-DIM-CASE
   COUNT-CHAR-CASE
   T-RES-WALK-CASE ;

\ ---- the word the chain cannot compile yet -----------------------------------
\ It names every capability it is waiting for rather than the first that stops
\ it, and the head of this file says in full what it is and which dot lands it.
: GAP-CASES ( -- )
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" CODEGEN--GAP-CAP:CALLS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO ;

public

\ Compile every corpus word the chain can express, declare the rest, and check
\ that between them they account for all of it. Runs after the old column, whose
\ rows the names are checked against.
: RUN ( -- )
   CODEGEN-GAP:RESET
   COVERED-CASES
   GAP-CASES
   CODEGEN-GAP:COVERAGE-CK ;

;package
