\ native-publish.f - the publication seam: what it writes, and what it refuses.
\ One concern: src/compiler/native/publish.f.
\
\ WHAT THIS SUITE HAS TO SHOW. Three things, and the third is the one the seam
\ exists for.
\
\   1. That a republished record really points at the emission. Not "the word
\      still answers correctly" - the old code answered correctly too - but that
\      the address in the record is the address the seam claimed, that the length
\      is the emission minus its return, and that the instruction word sitting at
\      that address is the emitter's own first instruction. A record pointing one
\      instruction into the routine would pass an answer test on some inputs and
\      none of these.
\   2. That a caller compiled afterwards reaches it. The engine resolves a call
\      when the CALLER is compiled, so a definition made after the republication
\      is what proves the new code is what a caller gets. It is compiled here by
\      `evaluate`, which is the ordinary interpret path.
\   3. That there is no second door to publication. REPUBLISH takes a name and
\      reads every byte it writes out of the sealed emission, and the sealed
\      emission only exists after the validator accepted an allocation for that
\      module - so the first case below asks it to publish with nothing sealed
\      and gets the emitter's own refusal. That is the nearest reachable
\      statement of "an unaccepted module cannot be published": the state that
\      would let it through is not constructible, because A64EMIT:EMIT is the
\      only word that seals and it refuses first.
\
\ AND THAT A REFUSAL CHANGES NOTHING. Every refusal case below is run with a live
\ sealed emission - the tempting moment to write - and the record of the word it
\ names is read before and after. A seam that wrote first and validated second
\ would pass the throw assertions and fail these.
\
\ THE CLAUSES WITH NO FIXTURE, AND WHY. E-NPUB-SIZE and E-NPUB-OFFSET are about
\ an emission that is not a whole number of instructions, or whose source map
\ points outside itself. No caller can build one: the emitter is the only source
\ of both numbers and it writes them from its own layout. They are checked by
\ mutating the emitter, which is recorded in the dot rather than here, on the
\ same terms as the other compiler mutations in this family.
\
\ E-NPUB-CLOBBER IS THE THIRD OF THOSE, and it is the one worth naming here
\ because of what it guards. What a routine destroys has to cover what everything
\ it calls destroys; the emitter counts that while it emits, and BRANCH-CK finds
\ the same callees a second way, by decoding the branches in the finished
\ instruction stream. No caller can present an emission whose two answers
\ disagree, so the fixture is a mutation: deleting the emitter's NOTE-CALLEE
\ makes every migration of a caller-of-a-caller refuse with this code, where
\ before the check existed the same mutation published a routine that destroyed
\ more than it said and the corpus crashed several words later.

require lib/test.f
require lib/fmt.f
require src/habu/layout.f
require src/compiler/native/publish.f
require test/compiler/native-source-fixture.f
require test/compiler/native-chain-fixture.f

package NPUB-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is the only way to compile a definition - a caller for the republished word -
\ from inside a test.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

4 constant REGS
4 constant INSN-BYTES
0 constant GLOBAL-WID             \ the wordlist the engine's own words live in

public

\ ---- the subjects ------------------------------------------------------------
\ Two words the engine compiled the ordinary way. The first is republished with
\ a routine that computes something DIFFERENT, so the answer alone tells the two
\ apart; the second is only ever named at a refusal, so its record is the one
\ that must not move. They are public so the caller this suite compiles after the
\ republication can name one of them from outside the package.
: PUB-SQ ( n -- n )
   dup + ;

: PUB-KEEP ( n -- n )
   3 * ;

\ The wordlist those two records live in. A word is a tail in a wordlist, and
\ these tails are in this package's export wordlist rather than the global one,
\ so it is read off the live namespace state rather than assumed to be zero.
get-current constant PUB-WID

private

\ ---- reading what the dictionary and the code now hold ----------------------
: REC ( ptr u8 n -- ptr a )
   PUB-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if E-NPUB-NAME throw then ;

: REC-START ( ptr u8 n -- n )
   REC XREF-START ;

: REC-LEN ( ptr u8 n -- n )
   REC XREF-LEN ;

\ One instruction word out of code space, little-endian, byte by byte: the seam
\ wrote it with a 32-bit poke and this reads it back the only way checked Habu
\ can read a byte of memory it was handed the address of.
TRUSTED: CODE-A ( n -- ptr u8 ) ;

: CODE-BYTE@ ( n -- n )
   CODE-A c@ ;

: CODE-WORD@ ( n -- n ) {: at:n :}
   at CODE-BYTE@
   at 1+ CODE-BYTE@ $8 lshift or
   at 2 + CODE-BYTE@ $10 lshift or
   at 3 + CODE-BYTE@ $18 lshift or ;

\ ---- the emission every case in this suite is run against --------------------
\ `PUB-SQ dup *` compiled under the convention a Habu word is entered through:
\ one argument out of data-stack slot zero of the caller's stack, one result back
\ into it. The name on the tape is the word the routine is going to be published
\ as, which is what a real migration's tape carries too.
here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER R-CTX IR-CTX:ctx
1 TYPED-BUFFER R-BLD IR-BUILD:builder

: CC ( -- IR-CTX:ctx )        0 R-CTX @ ;
: BB ( -- IR-BUILD:builder )  0 R-BLD @ ;

variable OLD-START
variable OLD-LEN

: COMPILE-SQ ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c NSRC:HIR-BUILDER 0 R-BLD !
   CC BB NSRC:MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   s" PUB-SQ dup *" NSRC:TEXT!
   CC BB NSRC:TAPE {: tp:IR-ARENA:arena :}
   CC NSRC:LEX
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   CC BB v p r 1 1 NELAB:COLON drop
   CC BB NSRC:TEXT$ 0 REGS 1 1 NFIX:RUN-HABU ;

\ ---- the cases ---------------------------------------------------------------
\ Before anything is compiled: there is no sealed emission, so there is nothing
\ to publish and the emitter says so. This is the whole no-second-door argument
\ as a test - REPUBLISH has no parameter through which bytes could arrive, so
\ with the emitter unsealed there is no publication to be had.
: NO-EMISSION-CASE ( -- )
   s" publication with nothing sealed is refused by the emitter" T-LABEL
   [: s" PUB-SQ" PUB-WID NPUB:REPUBLISH ;] E-A64EMIT-STATE TTHROWSQ ;

: RECORD-CASE ( -- )
   s" PUB-SQ" REC-START OLD-START !
   s" PUB-SQ" REC-LEN OLD-LEN !
   s" PUB-SQ" PUB-WID NPUB:REPUBLISH

   s" the record points at the address the seam claimed" T-LABEL
   s" PUB-SQ" REC-START
   s" PUB-SQ" PUB-WID NPUB:NEW-START T=

   s" the recorded length is the emission without its return" T-LABEL
   s" PUB-SQ" REC-LEN  A64EMIT:SIZE INSN-BYTES - T=

   s" and it is what the seam logged" T-LABEL
   s" PUB-SQ" REC-LEN  s" PUB-SQ" PUB-WID NPUB:NEW-LEN T=

   s" the first instruction at that address is the emitter's first" T-LABEL
   s" PUB-SQ" REC-START CODE-WORD@  0 A64EMIT:WORD@ T=

   s" the last instruction of the emission is one word past the record" T-LABEL
   s" PUB-SQ" REC-START  s" PUB-SQ" REC-LEN + CODE-WORD@
   A64EMIT:INSNS 1- A64EMIT:WORD@ T=

   s" the record the seam replaced is what it held before" T-LABEL
   s" PUB-SQ" PUB-WID NPUB:OLD-START OLD-START @ T=
   s" PUB-SQ" PUB-WID NPUB:OLD-LEN OLD-LEN @ T=

   s" the seam logged one republication" T-LABEL
   s" PUB-SQ" PUB-WID NPUB:REPUBLISHED? TTRUE
   s" PUB-KEEP" PUB-WID NPUB:REPUBLISHED? TFALSE ;

\ Every refusal below runs while the emission is still sealed, which is the one
\ moment the seam could write if it wrote before it validated.
: UNKNOWN-CASE ( -- )
   s" a name no record carries is refused" T-LABEL
   [: s" PUB-NOT-A-WORD" PUB-WID NPUB:REPUBLISH ;] E-NPUB-NAME TTHROWSQ ;

: RETIRED-CASE ( -- )
   s" : PUB-GONE ( -- n ) 5 ;" EV
   s" PUB-GONE" UNDEFINE-IF-DEFINED
   s" a retired record is refused" T-LABEL
   [: s" PUB-GONE" XREF-RETIRED-WL NPUB:REPUBLISH ;] E-NPUB-NAME TTHROWSQ ;

\ A package's own record holds a wordlist id in the cell a word holds its code
\ address in, so pointing it at code would not mean "this word's code moved".
: NAMESPACE-CASE ( -- )
   s" a package record is refused" T-LABEL
   [: s" NPUB" XREF-NAMESPACE-WL NPUB:REPUBLISH ;] E-NPUB-NAME TTHROWSQ ;

\ An immediate word's caller is the compiler, so republishing one would install
\ a routine to be RUN at compile time rather than called by the program.
: IMMEDIATE-CASE ( -- )
   s" an immediate word is refused" T-LABEL
   [: s" if" GLOBAL-WID NPUB:REPUBLISH ;] E-NPUB-NAME TTHROWSQ ;

\ An engine-internal word is one the interpreter itself refuses to enter. The
\ marking prim is marked internal by the engine's own sealing pass, so it is the
\ one such record a test can name.
: INTERNAL-CASE ( -- )
   s" an engine-internal word is refused" T-LABEL
   [: s" int-mark" GLOBAL-WID NPUB:REPUBLISH ;] E-NPUB-NAME TTHROWSQ ;

\ The code arena ends where the engine stops admitting definitions. Moving the
\ free slot up to that line and asking to publish there is refused; the slot is
\ put back afterwards, and it can be put back because the refusal happens before
\ anything is written.
variable SAVED-CP

: AT-CEILING ( -- )
   dbase@ REGION + $4000 - cp! ;

: ROOM-BODY ( -- )
   AT-CEILING
   [: s" PUB-KEEP" PUB-WID NPUB:REPUBLISH ;] E-NPUB-ROOM TTHROWSQ ;

: ROOM-CASE ( -- )
   s" an emission that does not fit under the arena's end reserve is refused" T-LABEL
   cp@ SAVED-CP !
   [: ROOM-BODY ;] catch {: rc:n :}
   SAVED-CP @ CODE-RECLAIM:TRUNCATE
   rc 0 <> if rc throw then ;

: UNTOUCHED-CASE ( -- )
   s" a refused word's record is exactly as it was" T-LABEL
   s" PUB-KEEP" REC-START OLD-START !
   s" PUB-KEEP" REC-LEN OLD-LEN !
   [: s" PUB-KEEP" XREF-NAMESPACE-WL NPUB:REPUBLISH ;] E-NPUB-NAME TTHROWSQ
   s" PUB-KEEP" REC-START OLD-START @ T=
   s" PUB-KEEP" REC-LEN OLD-LEN @ T= ;

\ ---- the slot the emission's branches were measured from ---------------------
\ A routine whose body calls another word carries a branch measured from the
\ address the routine itself was going to be written at, so publishing it
\ anywhere else would leave that branch pointing at the wrong instruction. The
\ seam is the one authority on where a routine lands, so it holds the address the
\ emitter was given against the slot it is claiming now.
\
\ THE CASE IS BUILT WITH A PLACEMENT THAT IS DELIBERATELY NOT THAT SLOT, which is
\ what a code pointer that moved between the emission and the publication would
\ look like from here. Four instructions past the free slot is enough: the seam
\ compares the two addresses and there is no tolerance in it.
\
\ AND THE POSITIVE HALF IS THE RECORD-CASE ABOVE, which publishes an emission
\ made with no placement at all - a routine with no such branch needs none - so
\ the two together say the check is on the pair and not on the emission alone.
16 constant PLACE-SKEW               \ four instructions past the slot the seam will claim

: PLACE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   NPUB:NEXT-SLOT PLACE-SKEW + A64EMIT:PLACE-AT
   c COMPILE-SQ
   s" PUB-KEEP" REC-START OLD-START !
   s" PUB-KEEP" REC-LEN OLD-LEN !

   s" an emission whose placement is not the claimed slot is refused" T-LABEL
   A64EMIT:PLACED? TTRUE
   A64EMIT:PLACEMENT  NPUB:NEXT-SLOT PLACE-SKEW +  T=
   [: s" PUB-KEEP" PUB-WID NPUB:REPUBLISH ;] E-NPUB-PLACE TTHROWSQ

   s" and the record it named is exactly as it was" T-LABEL
   s" PUB-KEEP" REC-START OLD-START @ T=
   s" PUB-KEEP" REC-LEN OLD-LEN @ T= ;

: PLACE-CASE ( -- )
   NFIX:BINDING [: PLACE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- what a refused publication leaves behind, measured ------------------------
\ THE CLAIM UNDER TEST. A publication either happens or it does not. Every
\ condition that can refuse is asked before the first byte moves, so a refusal at
\ ANY of them leaves the three things a publication writes exactly as it found
\ them: the code arena, the dictionary record, and the call map. The cases above
\ each check the record after their own refusal; these check all three, at every
\ fallible point, against a witness taken immediately before the attempt.
\
\ WHY ALL THREE AND NOT JUST THE RECORD. The defect this replaces was a refusal
\ that came AFTER a mutation - an overlong name returned E-NPUB-CAP from the
\ replacement log with the record already pointing at the new routine - and a
\ record-only check cannot see the other two. The call map matters most of the
\ three because nothing else in the system reads it until a snapshot is restored,
\ which is far away from anything that could report the damage.
\
\ THE WITNESS IS THE BYTES THEMSELVES. Not a count and not a flag: the words of
\ code space at the slot the publication would have claimed, the map bits over
\ that same span, the code pointer, and the record's two cells. A publication
\ that wrote anything at all moves one of them.
32 constant WIT-WORDS                \ words of the free slot the witness covers

create WIT-CODE WIT-WORDS cells allot
create WIT-MAP WIT-WORDS cells allot
variable WIT-CP
variable WIT-START
variable WIT-LEN

\ One bit of the region-to-text call map, read the way the publisher writes it:
\ by region word offset, out of the byte the map keeps it in.
TRUSTED: DATA-A ( -- ptr u8 )
   data-base ;

: MAP-BIT@ ( n -- n ) {: at:n :}
   at dbase@ - {: off:n :}
   DATA-A SNAP-RELOC:CALLMAP-OFF + off 5 rshift + c@
   off 2 rshift 7 and rshift 1 and ;

\ The record half of the witness is taken of a word that always exists, because
\ one of the points below names a word that does not - there is no record to read
\ for an unknown name, and the claim being tested is about the state a refusal
\ leaves, not about the name it refused. PUB-KEEP is never successfully
\ published by any of these points, so its record moving at all is the failure.
: WITNESS ( -- )
   cp@ WIT-CP !
   s" PUB-KEEP" REC-START WIT-START !
   s" PUB-KEEP" REC-LEN WIT-LEN !
   WIT-WORDS 0 ?do
      cp@ i 4 * +  {: at:n :}
      at CODE-WORD@  WIT-CODE i cells + !
      at MAP-BIT@    WIT-MAP i cells + !
   loop ;

: WITNESS-CK ( -- )
   cp@ WIT-CP @ T=
   s" PUB-KEEP" REC-START WIT-START @ T=
   s" PUB-KEEP" REC-LEN WIT-LEN @ T=
   WIT-WORDS 0 ?do
      WIT-CP @ i 4 * + {: at:n :}
      at CODE-WORD@  WIT-CODE i cells + @  T=
      at MAP-BIT@    WIT-MAP i cells + @  T=
   loop ;

\ One mutation point: take the witness, make the publication fail exactly there,
\ and hold the whole witness against what is there afterwards.
\
\ THE SUBJECT TRAVELS IN CELLS BECAUSE A QUOTATION IS NOT A CLOSURE. `[: ... ;]`
\ may not read the enclosing word's locals, and the publication has to happen
\ inside one so its throw can be caught and named. So the name and the wordlist
\ are put where the quotation's own body can read them.
128 BUFFER: PT-NAME
variable PT-U
variable PT-WID

: PT-NAME$ ( -- ptr u8 n )
   PT-NAME PT-U @ ;

: PT-PUBLISH ( -- )
   PT-NAME$ PT-WID @ NPUB:REPUBLISH ;

: POINT ( ptr u8 n n n -- ) {: a:ptr u:n wid:n code:n :}
   a PT-NAME u STR-LEN BYTE-COPY-LEN
   u PT-U !
   wid PT-WID !
   WITNESS
   [: PT-PUBLISH ;] code TTHROWSQ
   WITNESS-CK ;

\ A name the log cannot hold. NAME-MAX is 64, so this word's tail is longer than
\ a log row, and the refusal it earns is the exact one that used to arrive after
\ the record had already been retargeted.
: DEFINE-LONG ( -- )
   s" : PUB-NAME-THAT-IS-DELIBERATELY-LONGER-THAN-A-LOG-ROW-CAN-EVER-HOLD ( n -- n ) 2 * ;"
   EV ;

: LONG$ ( -- ptr u8 n )
   s" PUB-NAME-THAT-IS-DELIBERATELY-LONGER-THAN-A-LOG-ROW-CAN-EVER-HOLD" ;

: ATOMIC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c COMPILE-SQ

   s" a name the record resolves but the log cannot hold leaves everything"
   T-LABEL
   LONG$ GLOBAL-WID E-NPUB-CAP POINT

   s" an unknown name leaves everything" T-LABEL
   s" PUB-NO-SUCH-WORD" PUB-WID E-NPUB-NAME POINT

   s" a package record leaves everything" T-LABEL
   s" PUB-KEEP" XREF-NAMESPACE-WL E-NPUB-NAME POINT

   s" an immediate word leaves everything" T-LABEL
   s" IF" GLOBAL-WID E-NPUB-NAME POINT ;

\ The two points that need the code pointer moved out from under the attempt: a
\ slot with no room under the end reserve, and a placement that is not the slot
\ being claimed. Both restore the pointer afterwards, which they can do because
\ neither wrote anything - which is the claim.
: ROOM-POINT ( -- )
   cp@ SAVED-CP !
   s" a publication with no room leaves everything" T-LABEL
   AT-CEILING
   s" PUB-KEEP" PUB-WID E-NPUB-ROOM POINT
   SAVED-CP @ CODE-RECLAIM:TRUNCATE ;

: PLACE-POINT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   NPUB:NEXT-SLOT PLACE-SKEW + A64EMIT:PLACE-AT
   c COMPILE-SQ
   s" a placement that is not the claimed slot leaves everything" T-LABEL
   s" PUB-KEEP" PUB-WID E-NPUB-PLACE POINT ;

: ATOMIC-CASES ( -- )
   DEFINE-LONG
   NFIX:BINDING [: ATOMIC-BODY ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: drop ROOM-POINT ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: PLACE-POINT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- and what a publication that SUCCEEDS wrote --------------------------------
\ The other half of the same question. The window copies the emitter's buffer in
\ one go instead of poking each instruction at its own mapped offset, so "the
\ published bytes are the emission" is now a property of one bulk copy and is
\ worth reading back word for word. The map is read back too: a routine with no
\ call to engine text must leave no bit set anywhere in its span, which is what
\ says the window's clear really ran.
: BYTES-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c COMPILE-SQ
   NPUB:NEXT-SLOT {: at:n :}
   s" PUB-KEEP" PUB-WID NPUB:REPUBLISH

   s" every word of the published routine is the word the emitter sealed" T-LABEL
   A64EMIT:INSNS 0 ?do
      at i 4 * + CODE-WORD@  i A64EMIT:WORD@  T=
   loop

   s" the code pointer moved past exactly the emission" T-LABEL
   cp@ at A64EMIT:SIZE + T=

   s" and no word of it claims a call into engine text" T-LABEL
   A64EMIT:INSNS 0 ?do
      at i 4 * + MAP-BIT@ 0 T=
   loop ;

: BYTES-CASE ( -- )
   NFIX:BINDING [: BYTES-BODY ;] IR-CTX:WITH-CONTEXT ;

: SEALED-CASES ( -- )
   RECORD-CASE
   UNKNOWN-CASE
   RETIRED-CASE
   NAMESPACE-CASE
   IMMEDIATE-CASE
   INTERNAL-CASE
   ROOM-CASE
   UNTOUCHED-CASE ;

: BODY ( IR-CTX:ctx -- )
   COMPILE-SQ
   SEALED-CASES ;

\ Compiled after the republication, so the engine resolved this call against the
\ record the seam rewrote. `PUB-SQ` doubled its argument before and squares it
\ now, so the answer says which code the caller reached.
: CALLER-CASE ( -- )
   s" a definition compiled afterwards calls the republished code" T-LABEL
   s" : PUB-CALL ( n -- n ) NPUB-TEST:PUB-SQ ;" EV
   s" 7 PUB-CALL" EV-N 49 T=
   s" 11 PUB-CALL" EV-N 121 T=

   s" and the word answers the same when the interpreter enters it" T-LABEL
   s" 7 NPUB-TEST:PUB-SQ" EV-N 49 T=

   s" a word that was never republished is unchanged" T-LABEL
   s" 7 NPUB-TEST:PUB-KEEP" EV-N 21 T= ;

\ ---- the log holds one row per republished word, not 128 ----------------------
\ WHAT THIS IS FOR. The replacement log used to be a fixed table of 128 rows,
\ which is what the system republished when it was written. Once the clobber
\ record grew past its own fixed table this was the next ceiling under it: a run
\ that drove the cut's own entry over four hundred definitions published EXACTLY
\ 128 and was refused the 129th with E-NPUB-CAP, after every other refusal this
\ seam can make had accepted it. A log that cannot grow is a bound on how much of
\ a program the chain may publish, and this case is what says it no longer is one.
\
\ IT IS DRIVEN THROUGH THE SEAM AND NOT AROUND IT. Every row here is written by
\ NPUB:REPUBLISH, which is the only word that writes one - the log is private and
\ has no other door - so what the case fills is the real log by the real path.
\ The emission is the suite's own sealed one, published under one name after
\ another: a publication with no placement may be written at whatever slot the
\ seam claims (RECORD-CASE above is the same fact stated once), so one emission
\ can stand for the many definitions a cut publishes without paying for a chain
\ compile per row.
\
\ AND WHAT IT MEASURES IS NOT JUST THE COUNT. Every row is read back after the
\ storage has doubled twice, against the slot the seam claimed for it - the
\ routines are all one size, so row k belongs at base + k sizes and a lookup that
\ answered a neighbour would be exactly one routine out. The spellings are read
\ back too, because they live in a column of their own and a growth that smeared
\ it would answer one name's row for another: PUB-G1 and PUB-G10 share a prefix,
\ and neither the bare prefix nor a spelling one byte longer than a stored one may
\ be found.
\
\ AND THEN THE ROWS ARE GIVEN BACK, which is the whole of why the ceiling is a
\ bound on live routines rather than on how long the process has been running.
\ The reclamation is the engine's own FORGET, so the records go with the code and
\ nothing is left pointing into the space that was handed back. This case runs
\ last for that reason.
128 constant OLD-CEILING              \ the fixed table the log used to be
320 constant GROWN-ROWS               \ two doublings past the seed

64 BUFFER: G-NAME
variable G-NAME-U
variable G-BASE
variable G-SIZE
variable G-BEFORE
variable G-SQ-START
variable G-OLD-FIRST
variable G-OLD-LAST

: G-NAME$ ( -- ptr u8 n )
   G-NAME G-NAME-U @ ;

: G-NAME! ( n -- ) {: k:n :}
   SB-RESET
   s" PUB-G" SB-APPEND
   k FMT:SB-U
   SB$ {: a:ptr u:n :}
   a G-NAME u STR-LEN BYTE-COPY-LEN
   u G-NAME-U ! ;

\ The subjects. They are compiled by the ordinary interpret path, so each one is
\ a record the engine published in the global wordlist - which is what a
\ republication rewrites.
: G-DEF ( n -- ) {: k:n :}
   k G-NAME!
   SB-RESET
   s" : " SB-APPEND  G-NAME$ SB-APPEND  s"  ( n -- n ) 1 + ;" SB-APPEND
   SB$ EV ;

: G-DEFINE-ALL ( -- )
   GROWN-ROWS 0 ?do i G-DEF loop ;

: G-REC-START ( n -- n ) {: k:n :}
   k G-NAME!
   G-NAME$ GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if E-NPUB-NAME throw then
   XREF-START ;

\ Where the seam wrote row k's routine. One emission means one size, and the seam
\ moves the code pointer past exactly the emission it published (BYTES-CASE
\ above), so the slots are that size apart and this is an exact expectation
\ rather than a bound.
: G-AT ( n -- n ) {: k:n :}
   G-BASE @ k G-SIZE @ * + ;

: G-PUB ( n -- ) {: k:n :}
   k G-NAME!
   G-NAME$ GLOBAL-WID NPUB:REPUBLISH ;

: G-NEW-START ( n -- n ) {: k:n :}
   k G-NAME!
   G-NAME$ GLOBAL-WID NPUB:NEW-START ;

: G-KNOWN? ( n -- bool ) {: k:n :}
   k G-NAME!
   G-NAME$ GLOBAL-WID NPUB:REPUBLISHED? ;

: G-ROW-EXACT ( n -- ) {: k:n :}
   k G-NEW-START  k G-AT T=
   k G-NAME!
   G-NAME$ GLOBAL-WID NPUB:NEW-LEN  G-SIZE @ INSN-BYTES - T= ;

\ Every row against its own slot, in one answer: a lookup that answered a
\ neighbour's row, or a growth that lost or moved one, fails here whichever row
\ it was.
: G-ALL-EXACT? ( -- bool )
   true
   GROWN-ROWS 0 ?do
      i G-NEW-START  i G-AT <> if drop false leave then
   loop ;

\ The order the reclamation cut rests on: a publication's slot is above every
\ slot claimed before it, so the rows a floor takes away are the tail of the log.
: G-ORDERED? ( -- bool )
   true
   GROWN-ROWS 1 ?do
      i G-NEW-START  i 1- G-NEW-START <= if drop false leave then
   loop ;

: G-FILL ( -- )
   NPUB:REPUBLISHED G-BEFORE !
   A64EMIT:SIZE G-SIZE !
   NPUB:NEXT-SLOT G-BASE !
   s" PUB-SQ" PUB-WID NPUB:NEW-START G-SQ-START !
   0 G-REC-START G-OLD-FIRST !
   GROWN-ROWS 1- G-REC-START G-OLD-LAST !
   GROWN-ROWS 0 ?do i G-PUB loop ;

: G-CASES ( -- )
   s" the log holds one row per republished word, not 128" T-LABEL
   NPUB:REPUBLISHED  G-BEFORE @ GROWN-ROWS + T=
   GROWN-ROWS OLD-CEILING > TTRUE

   s" a row written before the storage grew still answers its own routine"
   T-LABEL
   0 G-ROW-EXACT
   OLD-CEILING 1- G-ROW-EXACT

   s" and so does the row the fixed table had no slot for at all" T-LABEL
   OLD-CEILING G-ROW-EXACT
   GROWN-ROWS 1- G-ROW-EXACT

   s" every row of the grown log answers the slot the seam claimed for it"
   T-LABEL
   G-ALL-EXACT? TTRUE

   s" and the rows are in publication order, which the reclamation cut rests on"
   T-LABEL
   G-ORDERED? TTRUE

   s" the code the old emitter produced is what the copied rows still report"
   T-LABEL
   0 G-NAME! G-NAME$ GLOBAL-WID NPUB:OLD-START G-OLD-FIRST @ T=
   GROWN-ROWS 1- G-NAME! G-NAME$ GLOBAL-WID NPUB:OLD-START G-OLD-LAST @ T=
   G-OLD-FIRST @ G-OLD-LAST @ T<>

   s" the spelling column is matched whole, by name and by wordlist" T-LABEL
   s" PUB-G1" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" PUB-G10" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" PUB-G" GLOBAL-WID NPUB:REPUBLISHED? TFALSE
   s" PUB-G1X" GLOBAL-WID NPUB:REPUBLISHED? TFALSE
   s" PUB-G3199" GLOBAL-WID NPUB:REPUBLISHED? TFALSE
   s" PUB-G1" XREF-NAMESPACE-WL NPUB:REPUBLISHED? TFALSE

   s" a row the log has no name for is still refused by name" T-LABEL
   [: s" PUB-G3199" GLOBAL-WID NPUB:NEW-START drop ;] E-NPUB-LOG TTHROWSQ

   s" and the log's own refusal still comes before anything moves" T-LABEL
   LONG$ GLOBAL-WID E-NPUB-CAP POINT ;

\ Reclaiming the code the rows describe. The engine's own FORGET is the door:
\ it retires the records from this name on and hands their code space back
\ through the one notice every address-keyed record is dropped by, so the rows
\ go with the routines rather than outliving them.
: G-RECLAIM-CASES ( -- )
   s" reclaiming the code they describe gives every one of those rows back"
   T-LABEL
   s" PUB-G0" FORGET-DEFS-FROM
   NPUB:REPUBLISHED G-BEFORE @ T=
   0 G-KNOWN? TFALSE
   OLD-CEILING G-KNOWN? TFALSE
   GROWN-ROWS 1- G-KNOWN? TFALSE

   s" and the rows below the floor are exactly as they were" T-LABEL
   s" PUB-SQ" PUB-WID NPUB:REPUBLISHED? TTRUE
   s" PUB-SQ" PUB-WID NPUB:NEW-START G-SQ-START @ T= ;

: GROW-BODY ( IR-CTX:ctx -- )
   COMPILE-SQ
   G-FILL
   G-CASES ;

: GROW-CASES ( -- )
   G-DEFINE-ALL
   NFIX:BINDING [: GROW-BODY ;] IR-CTX:WITH-CONTEXT
   G-RECLAIM-CASES ;

public

: RUN ( -- )
   T-RESET
   NO-EMISSION-CASE
   NFIX:BINDING [: BODY ;] IR-CTX:WITH-CONTEXT
   CALLER-CASE
   PLACE-CASE
   ATOMIC-CASES
   BYTES-CASE
   GROW-CASES
   T-REPORT ;

;package

NPUB-TEST:RUN
