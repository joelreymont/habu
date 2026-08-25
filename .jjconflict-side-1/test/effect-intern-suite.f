\ effect-intern-suite.f — the checker's effect-store node interner.
\
\ Run by the engine, like test/checker-scan-index-suite.f: every case is a
\ top-level interpret line, because the store and its interner are checker
\ internals that resolve only there, reached through named TRUSTED: shims.
\
\     bin/hb --load test/effect-intern-suite.f
\
\ WHAT IS UNDER TEST. E-COPY* used to write a fresh node for every term of every
\ recorded signature, so the store held 115,948 nodes carrying 1,593 distinct
\ shapes and grew 7.5MB over the compiler chain. It now interns: a finished
\ subterm at the top of the arena that matches one already below is discarded and
\ the older offset is returned (dot habu-the-effect-store-45bdc561).
\
\ THE FOUR THINGS THAT HAVE TO BE TRUE, and the sections that pin them:
\
\ 1. IDENTICAL MEANS SHARED, and the saving is real. Two words with the same
\    signature must resolve to the SAME stored row, and recording a signature the
\    store already holds must cost record headers and NOT ONE BYTE MORE. The
\    second half is what fails if the interner regresses to interning only
\    sometimes — the first half would still pass on the pair that happened to hit.
\
\ 2. DIFFERENT MEANS SEPARATE, field by field. Every cell E-KEY names is varied
\    on its own against a partner that agrees on everything else, so a key that
\    forgot a field shows up as two different signatures sharing one row. This is
\    the adversarial half: a hash-only table, or a compare over a shorter field
\    list than the hash, passes section 1 and fails here.
\
\ 3. A SHARED ROW BELONGS TO NOBODY. Deleting, redefining or truncating away one
\    of the words that share a row must not disturb the others, and a word whose
\    row was written by a record that has since been truncated away must not be
\    reachable through a stale table entry.
\
\ 4. THE TABLE NEVER NAMES A DEAD NODE — asserted as the WATERMARK and the
\    entries, not as an answer. The interner drops itself whenever it detects a
\    rewind it did not perform, so deleting the incremental truncation entirely
\    still gives right answers; only the mark and the live entry set tell the
\    repaired case from the rebuilt one (LESSONS.md, the index-rebuild lesson).

require lib/errors.f
require lib/string.f

\ Every definition here is a fixture helper, so they live in this file's own
\ package; the cases run as top-level interpret lines inside it, and the words
\ the cases define land in this package too.
package EFFINTERN-TEST

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

: T<> ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want = if
      T-FAIL s" assert: expected anything but " type want . cr
   then ;

: TTRUE ( bool -- ) if -1 else 0 then -1 T= ;
: TFALSE ( bool -- ) if -1 else 0 then 0 T= ;

\ ---------------------------------------------------------------------------
\ whitebox shims. The store, its interner and the layout they share are
\ checker-internal colon words; each is reached through one named boundary.
\ ---------------------------------------------------------------------------
TRUSTED: EIX-EVAL ( ptr u8 n -- ) evaluate ;
TRUSTED: EIX-UEND ( -- n ) UEND @ ;
TRUSTED: EIX-CELL ( n -- n ) USIGS-CELL-AT @ ;
TRUSTED: EIX-REC-BYTES ( -- n ) EFF-REC ;
TRUSTED: EIX-MIN-IN ( ptr u8 n -- n ) SIG-MIN-IN ;

\ the two row offsets of a NAME's active record, and -1 when the checker knows
\ no effect for it. This is the identity the whole suite is written against: two
\ words share a row exactly when these answer the same offset.
TRUSTED: EIX-DIN ( ptr u8 n -- n )
   FIND-SIG 0= IF -1 EXIT THEN
   FEP @ ER.DIN @ ;
TRUSTED: EIX-DOUT ( ptr u8 n -- n )
   FIND-SIG 0= IF -1 EXIT THEN
   FEP @ ER.DOUT @ ;

\ the interner's own state: how many nodes it is offering, the end it was last
\ made exact at, and each entry's node offset.
TRUSTED: EIX-N ( -- n ) UIX-N @ ;
TRUSTED: EIX-HI ( -- n ) UIX-HI @ ;
TRUSTED: EIX-ENTRY ( n -- n ) UIX-E-OFF UIX-E @ ;
\ E-KEY-N answers 0 for anything that is not a node this file writes, so it is
\ also the cheapest "is there still a node here" question, asked of the owner.
TRUSTED: EIX-KEY-N ( n -- n ) E-KEY-N ;
TRUSTED: EIX-TRUNCATE-FROM ( ptr u8 n -- ) CHECKER-USIGS-TRUNCATE-FROM-RAW ;

variable TC                     \ last caught throw code
variable M0                     \ store end before a measured load
variable IX

\ RECS-SINCE ( n -- n ) : records appended at or after offset `n`, by the store's
\ own chain. The measured growth of a load is compared against this, so the
\ assertion is "no node bytes" rather than a hard-coded byte count that a change
\ in how many records a definition publishes would make wrong for no reason.
: RECS-SINCE ( n -- n ) {: from:n :}
   0 from
   BEGIN dup EIX-CELL 0 <> WHILE
      EIX-CELL swap 1 + swap
   REPEAT drop ;

\ TABLE-LIVE ( -- ) : no entry names a byte at or above the store's end, and
\ every entry still names a node. Section 4's invariant, checked wherever the
\ store has just rewound.
: TABLE-LIVE ( -- )
   EIX-HI EIX-UEND > TFALSE
   0 BEGIN dup EIX-N < WHILE
      dup EIX-ENTRY EIX-UEND < TTRUE
      dup EIX-ENTRY EIX-KEY-N 0 T<>
      1 +
   REPEAT drop ;

\ ---------------------------------------------------------------------------
\ 1. IDENTICAL MEANS SHARED
\ ---------------------------------------------------------------------------

s" : EIXA ( n n -- n ) drop ;" EIX-EVAL
s" : EIXB ( n n -- n ) drop ;" EIX-EVAL
s" EIXA" EIX-DIN 0 T<>                         \ the comparison below is not vacuous
s" EIXA" EIX-DIN  s" EIXB" EIX-DIN T=          \ ... and the two words share one din row
s" EIXA" EIX-DOUT s" EIXB" EIX-DOUT T=
s" EIXA" EIX-MIN-IN 2 T=                       \ each still answers for itself
s" EIXB" EIX-MIN-IN 2 T=

\ The saving, measured: a third word with a signature the store already holds
\ costs its record headers and nothing else. A byte over that is a node the
\ interner failed to recognise.
EIX-UEND M0 !
s" : EIXC ( n n -- n ) drop ;" EIX-EVAL
EIX-UEND M0 @ -  M0 @ RECS-SINCE EIX-REC-BYTES *  T=
s" EIXC" EIX-DIN s" EIXA" EIX-DIN T=

\ A NEW signature costs more than its headers — the measurement above would pass
\ trivially if a definition never appended nodes at all. The new signature names
\ a family nothing else does, because a row of plain scalars is NOT new: whatever
\ arity is written here, some word in the engine's own prefix already has it, and
\ this case failed inside the gate pool for exactly that reason.
EIX-UEND M0 !
s" enum eixfresh alpha beta ;enum" EIX-EVAL
s" : EIXD ( eixfresh -- ) drop ;" EIX-EVAL
EIX-UEND M0 @ -  M0 @ RECS-SINCE EIX-REC-BYTES *  > TTRUE

\ ---------------------------------------------------------------------------
\ 2. DIFFERENT MEANS SEPARATE, field by field. Each pair below agrees on
\    everything except the one stored cell named in the comment; sharing a row
\    would mean E-KEY does not read that cell.
\ ---------------------------------------------------------------------------

\ EN-CON payload: two one-term rows of different concrete types
s" : EIXCON1 ( n -- ) drop ;" EIX-EVAL
s" : EIXCON2 ( bool -- ) drop ;" EIX-EVAL
s" EIXCON1" EIX-DIN  s" EIXCON2" EIX-DIN  T<>

\ EN-VAR id: identical tags, identical arity, the variables in a different order
s" : EIXVAR1 ( a b -- a b ) ;" EIX-EVAL
s" : EIXVAR2 ( a b -- b a ) swap ;" EIX-EVAL
s" EIXVAR1" EIX-DIN  s" EIXVAR2" EIX-DIN  T=       \ same din, by construction
s" EIXVAR1" EIX-DOUT s" EIXVAR2" EIX-DOUT T<>      \ ... and a different dout

\ EN-PTR wrapping: a pointer term against the bare term it points at
s" : EIXPTR1 ( ptr n -- ) drop ;" EIX-EVAL
s" : EIXPTR2 ( n -- ) drop ;" EIX-EVAL
s" EIXPTR1" EIX-DIN  s" EIXPTR2" EIX-DIN  T<>

\ EN-PUSH chain: the same term, one more of it
s" : EIXPUSH1 ( n -- ) drop ;" EIX-EVAL
s" : EIXPUSH2 ( n n -- ) drop drop ;" EIX-EVAL
s" EIXPUSH1" EIX-DIN  s" EIXPUSH2" EIX-DIN  T<>

\ EN-QUOT rows: quotation arguments differing in their own din, then their dout
s" : EIXQ1 ( [ -- ] -- ) drop ;" EIX-EVAL
s" : EIXQ2 ( [ n -- ] -- ) drop ;" EIX-EVAL
s" : EIXQ3 ( [ -- n ] -- ) drop ;" EIX-EVAL
s" EIXQ1" EIX-DIN  s" EIXQ2" EIX-DIN  T<>
s" EIXQ1" EIX-DIN  s" EIXQ3" EIX-DIN  T<>
s" EIXQ2" EIX-DIN  s" EIXQ3" EIX-DIN  T<>

\ EN-PARAM: the family's own name, and then its argument, are both key
s" enum eixcol red green blue ;enum" EIX-EVAL
s" : EIXP1 ( eixcol -- ) drop ;" EIX-EVAL
s" : EIXP2 ( n -- ) drop ;" EIX-EVAL
s" EIXP1" EIX-DIN  s" EIXP2" EIX-DIN  T<>
s" : EIXP3 ( eixcol -- ) drop ;" EIX-EVAL
s" EIXP1" EIX-DIN  s" EIXP3" EIX-DIN  T=           \ ... and the same family shares again

\ ---------------------------------------------------------------------------
\ 3. A SHARED ROW BELONGS TO NOBODY
\ ---------------------------------------------------------------------------

\ deleting one sharer leaves the others answering
s" : EIXS1 ( n n -- n ) drop ;" EIX-EVAL
s" : EIXS2 ( n n -- n ) drop ;" EIX-EVAL
s" EIXS1" EIX-DIN s" EIXS2" EIX-DIN T=
s" undefine EIXS1" EIX-EVAL
s" EIXS1" EIX-MIN-IN -1 T=                         \ gone
s" EIXS2" EIX-MIN-IN 2 T=                          \ the sharer is untouched
s" EIXS2" EIX-DIN 0 T<>
s" : EIXS3 ( n n -- n ) EIXS2 ;" EIX-EVAL          \ ... and still certifies a caller
s" EIXS3" EIX-MIN-IN 2 T=

\ redefining one sharer with a different signature moves only that word
s" undefine EIXS2 : EIXS2 ( n -- n ) ;" EIX-EVAL
s" EIXS2" EIX-MIN-IN 1 T=
s" EIXS2" EIX-DIN  s" EIXS3" EIX-DIN  T<>

\ ---------------------------------------------------------------------------
\ 4. THE TABLE NEVER NAMES A DEAD NODE
\ ---------------------------------------------------------------------------

TABLE-LIVE                                         \ ... after everything above

\ a truncation back to a marker word discards every record above it, and with
\ them every node those records wrote. The entries that named them have to go at
\ the same seam, and the mark has to come down with the store.
s" : EIXMARK ( -- ) ;" EIX-EVAL
s" enum eixtrunc red green ;enum" EIX-EVAL         \ again fresh, so nodes really die
s" : EIXT1 ( eixtrunc -- ) drop ;" EIX-EVAL
s" EIXT1" EIX-DIN IX !
IX @ 0 T<>
EIX-UEND M0 !
s" EIXMARK" EIX-TRUNCATE-FROM
EIX-UEND M0 @ < TTRUE                              \ the store really did rewind
TABLE-LIVE

\ and the next definition's row is a LIVE node below the store's end, not an
\ offset the truncation left behind: a table that kept its discarded entries
\ answers with one of them, and the store then writes a different node over it.
s" : EIXT2 ( n n n n n -- ) drop drop drop drop drop ;" EIX-EVAL
s" EIXT2" EIX-DIN EIX-UEND < TTRUE
s" EIXT2" EIX-DIN EIX-KEY-N 0 T<>
TABLE-LIVE

\ a rolled-back candidate leaves neither store nor table behind it
TRUSTED: EIX-BAD-DEF ( -- ) s" : EIXBAD ( n -- n ) drop ;" evaluate ;
EIX-UEND M0 !
' EIX-BAD-DEF catch TC !
TC @ 0 T<>                                         \ the definition really was rejected
EIX-UEND M0 @ <= TTRUE
EIX-HI EIX-UEND > TFALSE
TABLE-LIVE

\ the store still answers correctly afterwards
s" EIXA" EIX-MIN-IN 2 T=
s" EIXT2" EIX-MIN-IN 5 T=

\ A REWIND NO SEAM PERFORMED. src/habu/hide.f's refresh prelude assigns UEND and
\ writes the terminator directly, so the store can move under the table without
\ USIGS-RESTORE-END ever being called. The append choke point is where that has
\ to be noticed, and the case that proves it is a record with NO NODES: an
\ `undefine` appends a record and copies nothing, so the interner's own entry
\ path never runs and only E-REC-START's sync is left to see the rewind. Read as
\ the MARK rather than as an answer - a table that is merely rebuilt later
\ answers correctly too, and that is exactly what this must tell apart.
TRUSTED: EIX-RAW-REWIND ( n -- ) UEND ! UTERM! ;

s" : EIXOLD ( n -- ) drop ;" EIX-EVAL
EIX-UEND M0 !
s" enum eixrw one two ;enum" EIX-EVAL              \ a family nothing else names
s" : EIXRW ( eixrw -- ) drop ;" EIX-EVAL           \ ... so its nodes are new
EIX-UEND M0 @ > TTRUE
M0 @ EIX-RAW-REWIND
EIX-HI EIX-UEND > TTRUE                            \ the mark is stale, deliberately
s" undefine EIXOLD" EIX-EVAL                       \ one record, not one node
EIX-HI EIX-UEND > TFALSE                           \ the append seam noticed
TABLE-LIVE

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" effect-intern-suite: failures" 1 die ;
REPORT

;package
