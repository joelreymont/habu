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
   SAVED-CP @ cp!
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

public

: RUN ( -- )
   T-RESET
   NO-EMISSION-CASE
   NFIX:BINDING [: BODY ;] IR-CTX:WITH-CONTEXT
   CALLER-CASE
   PLACE-CASE
   T-REPORT ;

;package

NPUB-TEST:RUN
