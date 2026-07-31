\ native-feed.f - checked tests for the stage N0 source-tape producer.
\
\ Every fixture here compiles a real colon definition through the production
\ path: `evaluate` hands the text to the engine, the engine compiles it and
\ calls its check hook at the `;`, and the checker's own reader fills the tape
\ while it consumes the tokens. Nothing in this file lexes anything, so a tape
\ that disagreed with what the engine read would show up as a wrong row rather
\ than as two lexers agreeing with each other.
\
\ What it proves: the token count, spellings, kinds, modes, spans and literal
\ of a real definition; exactly which one-byte edits the sealed tape's own
\ digest can and cannot see, and which of those the source registry's content
\ digest catches instead; that the text the tape records is the definition the
\ engine reconstructed, so a backslash comment and the original whitespace are
\ not in it; that a spelling hidden in a parenthesised comment or inside a
\ string payload never becomes a row, because the reader never consumes it;
\ and that every refusal of the producer's state machine fires by name.

require lib/test.f
require src/compiler/native/feed.f

package NFEED-TEST
private

\ ---- fixtures ----------------------------------------------------------------
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the one boundary these tests need ---------------------------------------
\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is the only way to put a definition through the real compile path from inside
\ a test. Three named boundaries, the shape test/enum-decl-suite.f already uses:
\ one that compiles a definition, one that evaluates an expression for the one
\ value the engine leaves, and one that catches whatever the compile threw.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;
TRUSTED: EV-CATCH ( ptr u8 n -- n ) ['] EV catch ;

\ ---- the units under test ----------------------------------------------------
\ Two slots, because every digest question is a question about a pair: what two
\ compilations of texts that differ in one byte produce.
2 constant UNITS

here CELL 1- and CELL swap - CELL 1- and allot
UNITS TYPED-BUFFER T-KEY IR-ID:ir-module-key
UNITS TYPED-BUFFER T-MOD IR-BUILD:module
UNITS TYPED-BUFFER T-TAPE IR-ARENA:view
create T-VERDICT UNITS cells allot   \ the verdict each unit answered

\ The buffer a unit keeps its scanned text in. A unit is opened with it, the
\ producer copies the reader's text into it, and a case reads the recorded
\ length back off the frozen source registry - so the bytes stay readable after
\ the engine's own scratch has been refilled by the next compilation.
256 constant TEXT-CAP
create UTXT TEXT-CAP allot

: KEY@ ( n -- IR-ID:ir-module-key )  T-KEY @ ;
: MOD@ ( n -- IR-BUILD:module )      T-MOD @ ;
: TAPE@ ( n -- IR-ARENA:view )       T-TAPE @ ;
: VERDICT@ ( n -- n )                cells T-VERDICT + @ ;
: SRC@ ( n -- IR-ARENA:view )        MOD@ IR-BUILD:FSOURCES ;

\ The source the unit registered, read off the tape rather than carried
\ alongside it: every row's span names the source it spans into, so the
\ identity a case asks the registry about is the one the recorded rows use.
: SRC-ID ( n -- IR-ID:ir-source-id )
   {: slot:n :}
   slot TAPE@ slot KEY@ 0 NTAPE:SPAN@ IR-SOURCE:SPAN-SRC ;

: NEW-BLD ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c s" habu" 1 0 IR-BUILD:NEW-BUILDER ;

\ Compile one definition through the production path with the tape recording,
\ then freeze the module the tokens were recorded into and park everything a
\ case reads back. The tape ceiling is generous on purpose: a definition longer
\ than it is refused by NTAPE, and no fixture here is meant to reach it.
: REC ( IR-CTX:ctx ptr u8 n n -- )
   {: c:IR-CTX:ctx a:ptr u:n slot:n :}
   c NEW-BLD {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY slot T-KEY !
   c b IR-BUILD:MODULE-KEY 32 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c b tp UTXT TEXT-CAP NFEED:BEGIN-UNIT
   a u EV
   NFEED:END-UNIT  slot cells T-VERDICT + !  slot T-TAPE !
   c b IR-BUILD:FREEZE slot T-MOD ! ;

\ ---- reading a recorded tape back --------------------------------------------
: TOKENS ( n -- n )
   TAPE@ NTAPE:TOKENS ;

: SPELL ( n n -- IR-ID:ir-symbol-id )
   {: slot:n i:n :}
   slot TAPE@ slot KEY@ i NTAPE:SPELL@ ;

\ Is row i spelled these bytes? The question is asked of the module's frozen
\ interner, so it is an identity comparison against the symbol the row names -
\ not a search for the text in the source.
: SPELL-IS? ( n n ptr u8 n -- bool )
   {: slot:n i:n a:ptr u:n :}
   slot MOD@ IR-BUILD:FSYM-POOL  slot MOD@ IR-BUILD:FSYM-ROWS
   slot i SPELL  a u IR-SYM:FEQ? ;

\ Is ANY row spelled these bytes? This is what a hostile fixture asks: text
\ that appears in the source but never as a consumed token must not be able to
\ name a row anywhere on the tape.
: ANY-SPELL? ( n ptr u8 n -- bool )
   {: slot:n a:ptr u:n :}
   false
   slot TOKENS 0 ?do
      slot i a u SPELL-IS? or
   loop ;

: KIND-IS? ( n n NTAPE:kind -- bool )
   {: slot:n i:n k:NTAPE:kind :}
   slot TAPE@ i NTAPE:KIND@ k NTAPE-KIND:EQ ;

: MODE-IS? ( n n NTAPE:mode -- bool )
   {: slot:n i:n m:NTAPE:mode :}
   slot TAPE@ i NTAPE:MODE@ m NTAPE-MODE:EQ ;

: SPAN-START ( n n -- n )
   {: slot:n i:n :}
   slot TAPE@ slot KEY@ i NTAPE:SPAN@ IR-SOURCE:SPAN-START ;

: SPAN-LEN ( n n -- n )
   {: slot:n i:n :}
   slot TAPE@ slot KEY@ i NTAPE:SPAN@ IR-SOURCE:SPAN-LEN ;

: LIT ( n n -- n )
   {: slot:n i:n :}
   slot TAPE@ i NTAPE:LIT@ ;

: LOCAL-SPELL ( n n -- n )
   SPELL IR-ID:SYMBOL-LOCAL ;

\ How many bytes the reader actually handed over, as the registry recorded them.
: SRC-LEN ( n -- n )
   {: slot:n :}
   slot SRC@ slot SRC-ID IR-SOURCE:FLEN@ ;

\ The registry's own content digest over the bytes this unit registered. It is
\ the authority on the bytes; the tape digest is the authority on the cells.
: SRC-DIGEST ( n -- CDIGEST:digest )
   {: slot:n :}
   slot SRC@ slot SRC-ID IR-SOURCE:FDIGEST@ ;

: SAME-TAPE? ( -- bool )
   0 TAPE@ NTAPE:DIGEST  1 TAPE@ NTAPE:DIGEST  CDIGEST-DIGEST:EQ ;

: SAME-TEXT? ( -- bool )
   0 SRC-DIGEST  1 SRC-DIGEST  CDIGEST-DIGEST:EQ ;

\ ---- the token grid of a real definition -------------------------------------
\ One definition of the Wave 2 straight-line slice, recorded while the engine
\ compiled it. The engine hands the checker the definition it reconstructed -
\ the name, the declared signature and the body - so the five tokens are the
\ name and the four body words, the signature is not a token, and the offsets
\ are offsets into that text.
: GRID-A-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   c s" : NF-SQUARE ( n -- n ) dup * 3 + ;" 0 REC
   0 TOKENS
   0 VERDICT@
   0 0 SPAN-START
   0 0 SPAN-LEN
   0 3 LIT ;

: GRID-A-CASE ( -- )
   s" a real colon definition records one row per consumed token" T-LABEL
   BND [: GRID-A-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= 9 T= 0 T= -1 T= 5 T= ;

: GRID-B-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-CUBE ( n -- n ) dup dup * * ;" 0 REC
   0 0 s" NF-CUBE" SPELL-IS?
   0 1 s" dup" SPELL-IS?
   0 2 s" dup" SPELL-IS?
   0 3 s" *" SPELL-IS?
   0 4 s" *" SPELL-IS? ;

: GRID-B-CASE ( -- )
   s" every row is spelled the token the reader consumed" T-LABEL
   BND [: GRID-B-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ The kinds and the modes. `:` runs from the outer interpreter and parses the
\ defined name before the parser switches to compiling, so the name token was
\ read while interpreting and every body token while compiling - and that is
\ what the tape says, rather than what this test assumes.
: GRID-C-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-BUMP ( n -- n ) 7 + ;" 0 REC
   0 0 NTAPE-KIND:NAME KIND-IS?
   0 1 NTAPE-KIND:INT-LITERAL KIND-IS?
   0 2 NTAPE-KIND:NAME KIND-IS?
   0 0 NTAPE-MODE:INTERPRETING MODE-IS?
   0 1 NTAPE-MODE:COMPILING MODE-IS? ;

: GRID-C-CASE ( -- )
   s" a token records the kind and the parser mode it was consumed in" T-LABEL
   BND [: GRID-C-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ The recorded literal is the value the engine itself pushed. The definition is
\ compiled with the tape recording and then RUN: what it computes over a known
\ input pins the value the engine's own number parser read, and the tape has to
\ carry that same value or this case goes red.
: LIT-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c s" : NF-ADD41 ( n -- n ) 41 + ;" 0 REC
   0 1 LIT
   s" 1 NF-ADD41 1 -" EV-N ;

: LIT-CASE ( -- )
   s" the recorded literal is the value the engine pushed" T-LABEL
   BND [: LIT-BODY ;] IR-CTX:WITH-CONTEXT
   41 T= 41 T= ;

\ Two rows can name the same spelling and must still be two rows: the interner
\ deduplicates the name, the tape does not deduplicate the token.
: REPEAT-BODY ( IR-CTX:ctx -- n bool bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-TWICE ( n -- n ) dup + dup + ;" 0 REC
   0 TOKENS
   0 1 LOCAL-SPELL  0 3 LOCAL-SPELL  =
   0 1 SPAN-START   0 3 SPAN-START   <> ;

: REPEAT-CASE ( -- )
   s" a repeated spelling is two rows with one symbol and two spans" T-LABEL
   BND [: REPEAT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 5 T= ;

\ ---- what the reader hands over ----------------------------------------------
\ The tape's source is the definition the engine reconstructed, not the file's
\ bytes: a backslash comment is gone before the checker sees anything, and runs
\ of whitespace have collapsed to one space. The recorded length says so
\ exactly - `NF-TRIM ( -- n ) 5 ` is nineteen bytes however the definition was
\ laid out - and a stage that wants the file's bytes has to read the file.
: TRIM-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c S\" : NF-TRIM ( -- n )   \\ a backslash comment, never a token\n    5 ;" 0 REC
   0 SRC-LEN
   0 TOKENS ;

: TRIM-CASE ( -- )
   s" the recorded source is the reconstructed definition" T-LABEL
   BND [: TRIM-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 19 T= ;

\ ---- the text the unit kept --------------------------------------------------
\ The buffer a unit is opened with has to still hold the recorded definition
\ after the engine has compiled something else, because that is the whole reason
\ for copying: IR-SOURCE stores a length and a digest, never the bytes, and the
\ text the reader handed over lives in the engine's own scratch. So this case
\ records one definition, compiles a SECOND one outside any unit, and only then
\ asks whether the buffer still digests to what the registry recorded. Delete the
\ copy in NFEED:ON-SCAN and this goes red, because the registry would then be
\ bound to bytes nobody kept.
: KEPT-DIGEST? ( n -- bool )
   {: slot:n :}
   slot SRC-DIGEST
   UTXT slot SRC-LEN CDIGEST:COMPUTE
   CDIGEST-DIGEST:EQ ;

: KEPT-BODY ( IR-CTX:ctx -- n bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-KEPT ( n -- n ) 4 * ;" 0 REC
   s" : NF-AFTER ( n -- n ) 9 + ;" EV
   0 SRC-LEN
   0 KEPT-DIGEST? ;

: KEPT-CASE ( -- )
   s" the unit's buffer still holds the recorded text after a later compilation" T-LABEL
   BND [: KEPT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 23 T= ;

\ ---- hostile fixtures --------------------------------------------------------
\ A spelling inside a parenthesised comment is text the reader steps over, so
\ it must not name a row. The count is asserted too: "no row is spelled it" is
\ only worth something if the rows that ARE there are the right ones.
: HIDE-COMMENT-BODY ( IR-CTX:ctx -- n bool bool bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-HIDEC ( n -- n ) ( zdup 77 ) 3 + ;" 0 REC
   0 TOKENS
   0 s" zdup" ANY-SPELL?
   0 s" 77" ANY-SPELL?
   0 1 s" 3" SPELL-IS? ;

: HIDE-COMMENT-CASE ( -- )
   s" a spelling hidden in a comment never becomes a row" T-LABEL
   BND [: HIDE-COMMENT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE TFALSE 3 T= ;

\ The same for a string payload. The opener IS a token - the reader consumes it
\ and steps over the bytes behind it - so the tape says `s"` was read and says
\ nothing about the body, which is the truth about what this reader consumed.
: HIDE-STRING-BODY ( IR-CTX:ctx -- n bool bool bool )
   {: c:IR-CTX:ctx :}
   c S\" : NF-HIDES ( -- n ) s\" zdup z77\" 2drop 5 ;" 0 REC
   0 TOKENS
   0 s" zdup" ANY-SPELL?
   0 s" z77" ANY-SPELL?
   0 1 S\" s\"" SPELL-IS? ;

: HIDE-STRING-CASE ( -- )
   s" a spelling hidden in a string payload never becomes a row" T-LABEL
   BND [: HIDE-STRING-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE TFALSE 4 T= ;

\ ---- what one changed byte moves ---------------------------------------------
\ A byte inside a literal moves the tape digest. The pair is built so that the
\ literal is the only cell that can differ: the two names are the same length,
\ so every span is the same, each name is its module's first symbol, so every
\ spelling ordinal is the same, and the kinds and modes are the same. The
\ literal cell is what is left.
: LITBYTE-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-LVA ( -- n ) 1 ;" 0 REC
   c s" : NF-LVB ( -- n ) 2 ;" 1 REC
   0 TOKENS 1 TOKENS =
   0 1 SPAN-START 1 1 SPAN-START =
   0 1 LOCAL-SPELL 1 1 LOCAL-SPELL =
   0 1 LIT 1 1 LIT <>
   SAME-TAPE? ;

: LITBYTE-CASE ( -- )
   s" a changed literal byte changes the tape digest" T-LABEL
   BND [: LITBYTE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE TTRUE TTRUE ;

\ A byte inside a NAME does not move the tape digest, and that is not a defect
\ to paper over: the tape stores a spelling as its module-local symbol ordinal,
\ and each of these two names is its own module's first symbol, so the two
\ tapes are cell for cell identical. The registry's content digest is what
\ tells them apart, so a stage that needs to know WHICH bytes were read asks
\ the registry and never the tape - that is what instruction selection's own
\ digest check does in production (A64SEL:SOURCE!), and the reasoning behind
\ it is recorded in LESSONS.md, "A digest over interned identities cannot see
\ a spelling".
: NAMEBYTE-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-NMA ( -- n ) 1 ;" 0 REC
   c s" : NF-NMB ( -- n ) 1 ;" 1 REC
   SAME-TAPE?
   SAME-TEXT? ;

: NAMEBYTE-CASE ( -- )
   s" a changed name byte moves only the source digest" T-LABEL
   BND [: NAMEBYTE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

\ A byte inside a parenthesised comment behaves the same way, for the same
\ reason: the reader consumes no token there, so no cell of the tape moves,
\ while the bytes it read did change and the content digest says so.
: COMMENTBYTE-BODY ( IR-CTX:ctx -- bool bool bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-CMA ( -- n ) ( aa ) 1 ;" 0 REC
   c s" : NF-CMB ( -- n ) ( ab ) 1 ;" 1 REC
   0 TOKENS 1 TOKENS =
   SAME-TAPE?
   SAME-TEXT? ;

: COMMENTBYTE-CASE ( -- )
   s" a changed comment byte moves only the source digest" T-LABEL
   BND [: COMMENTBYTE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE ;

\ A token added to the body moves the tape digest through the token count and
\ every span after it.
: SHAPEBYTE-BODY ( IR-CTX:ctx -- n n bool )
   {: c:IR-CTX:ctx :}
   c s" : NF-SHA ( -- n ) 1 ;" 0 REC
   c s" : NF-SHB ( -- n ) 1 1 + ;" 1 REC
   0 TOKENS
   1 TOKENS
   SAME-TAPE? ;

: SHAPEBYTE-CASE ( -- )
   s" an added token changes the tape digest" T-LABEL
   BND [: SHAPEBYTE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE 4 T= 2 T= ;

\ ---- the refusals ------------------------------------------------------------
\ A refusal is raised INSIDE the context and rethrown outside it. A throw that
\ escapes IR-CTX:WITH-CONTEXT abandons the context, and an abandoned context
\ keeps its arena registry slots until an enclosing live context leaves - with
\ no enclosing context, that is never, and the next case runs out of slots.
\ Every refusal case below therefore answers its throw code and rethrows past
\ the context boundary.
\
\ A scratch unit, opened over a module nothing else reads. The three handles
\ are parked rather than kept in locals because a refusal case has to reach
\ them from inside a quotation under `catch`, and a local is not visible there.
here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER U-CTX IR-CTX:ctx
1 TYPED-BUFFER U-BLD IR-BUILD:builder
1 TYPED-BUFFER U-TP IR-ARENA:arena

: OPEN-SCRATCH ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c NEW-BLD {: b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY 32 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c 0 U-CTX !  b 0 U-BLD !  tp 0 U-TP !
   c b tp UTXT TEXT-CAP NFEED:BEGIN-UNIT ;

\ A unit opened with a buffer too small for the definition it is about to see.
\ Eight bytes is past the name alone, so the refusal is the producer's own and
\ not a zero-length special case.
8 constant TINY-CAP
create UTXT-TINY TINY-CAP allot

: OPEN-TINY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c NEW-BLD {: b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY 32 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c b tp UTXT-TINY TINY-CAP NFEED:BEGIN-UNIT ;

: REOPEN-SCRATCH ( -- )
   0 U-CTX @  0 U-BLD @  0 U-TP @  UTXT TEXT-CAP NFEED:BEGIN-UNIT ;

\ One unit is one scan. A second definition inside the same unit is a second
\ token stream, and blending the two into one tape is exactly what this refusal
\ makes impossible.
: TWO-DEFS-BODY ( IR-CTX:ctx -- n )
   OPEN-SCRATCH
   s" : NF-TWO1 ( -- n ) 1 ;" EV
   s" : NF-TWO2 ( -- n ) 2 ;" EV-CATCH {: rc:n :}
   NFEED:ABANDON-UNIT
   rc ;

: TWO-DEFS ( -- )
   BND [: TWO-DEFS-BODY ;] IR-CTX:WITH-CONTEXT throw ;

\ A unit that never reached a verdict has nothing to answer.
: END-QUIET ( -- )
   NFEED:END-UNIT drop drop ;

: NO-SCAN-BODY ( IR-CTX:ctx -- n )
   OPEN-SCRATCH
   [: END-QUIET ;] catch {: rc:n :}
   NFEED:ABANDON-UNIT
   rc ;

: NO-SCAN ( -- )
   BND [: NO-SCAN-BODY ;] IR-CTX:WITH-CONTEXT throw ;

\ A second unit over a live one is refused before the reader is armed a second
\ time, so the checker never sees two observers competing for one scan.
: TWO-UNITS-BODY ( IR-CTX:ctx -- n )
   OPEN-SCRATCH
   [: REOPEN-SCRATCH ;] catch {: rc:n :}
   NFEED:ABANDON-UNIT
   rc ;

: TWO-UNITS ( -- )
   BND [: TWO-UNITS-BODY ;] IR-CTX:WITH-CONTEXT throw ;

\ A literal this stage cannot record honestly is refused rather than recorded
\ as something else: a hexadecimal spelling has no value this producer can read
\ back, and a float has no tape kind at all.
: HEX-BODY ( IR-CTX:ctx -- n )
   OPEN-SCRATCH
   s" : NF-HEX ( -- n ) $FF ;" EV-CATCH {: rc:n :}
   NFEED:ABANDON-UNIT
   rc ;

: HEX ( -- )
   BND [: HEX-BODY ;] IR-CTX:WITH-CONTEXT throw ;

: REAL-BODY ( IR-CTX:ctx -- n )
   OPEN-SCRATCH
   s" : NF-REAL ( -- r ) 1.5 ;" EV-CATCH {: rc:n :}
   NFEED:ABANDON-UNIT
   rc ;

: REAL ( -- )
   BND [: REAL-BODY ;] IR-CTX:WITH-CONTEXT throw ;

\ A definition whose reconstructed text does not fit the buffer the unit was
\ opened with. The producer refuses the scan rather than recording spans into
\ bytes it could not keep.
: BIGTEXT-BODY ( IR-CTX:ctx -- n )
   OPEN-TINY
   s" : NF-TOOBIG ( n -- n ) 2 * ;" EV-CATCH {: rc:n :}
   NFEED:ABANDON-UNIT
   rc ;

: BIGTEXT ( -- )
   BND [: BIGTEXT-BODY ;] IR-CTX:WITH-CONTEXT throw ;

\ A token event that lies about where its bytes were read is refused. The event
\ surface is reachable only from an unchecked boundary - no checked caller can
\ name it, because it carries no prim axiom - so this fixture is the forge that
\ the byte check exists for: the offset says one thing and the bytes say
\ another, and a span nobody compared would otherwise enter the tape.
TRUSTED: FAKE-SCAN ( ptr u8 n -- ) CHECKER-TAPE:SCAN ;
TRUSTED: FAKE-TOKEN ( ptr u8 n n n -- ) CHECKER-TAPE:TOKEN ;

: LIE ( -- )
   s" NF-LIAR 1" FAKE-SCAN
   s" NF-LIAR" 2 0 FAKE-TOKEN ;

: LIAR-BODY ( IR-CTX:ctx -- n )
   OPEN-SCRATCH
   [: LIE ;] catch {: rc:n :}
   NFEED:ABANDON-UNIT
   rc ;

: LIAR ( -- )
   BND [: LIAR-BODY ;] IR-CTX:WITH-CONTEXT throw ;

\ A tape of one module and a builder of another are two halves of two different
\ modules, and the first token dies on the tape's own owner check rather than
\ recording a row against a registry that never registered it.
: XMOD-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c NEW-BLD {: b:IR-BUILD:builder :}
   c NEW-BLD {: b2:IR-BUILD:builder :}
   c b2 IR-BUILD:MODULE-KEY 32 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c b tp UTXT TEXT-CAP NFEED:BEGIN-UNIT
   s" : NF-XMOD ( -- n ) 1 ;" EV-CATCH {: rc:n :}
   NFEED:ABANDON-UNIT
   rc ;

: XMOD ( -- )
   BND [: XMOD-BODY ;] IR-CTX:WITH-CONTEXT throw ;

\ After a refusal a new unit opens and records normally: the state machine is
\ left clean by ABANDON-UNIT, not by luck.
: RECOVER-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c OPEN-SCRATCH
   s" : NF-RCV1 ( -- n ) $FF ;" EV-CATCH drop
   NFEED:ABANDON-UNIT
   c s" : NF-RCV2 ( -- n ) 6 ;" 0 REC
   0 TOKENS ;

: RECOVER-CASE ( -- )
   s" a unit opens and records after a refused one" T-LABEL
   BND [: RECOVER-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= ;

: REFUSE-CASES ( -- )
   s" a second definition inside one unit is refused" T-LABEL
   [: TWO-DEFS ;] E-NFEED-SCAN TTHROWSQ
   s" a unit that never scanned has no result" T-LABEL
   [: NO-SCAN ;] E-NFEED-STATE TTHROWSQ
   s" a second unit over a live one is refused" T-LABEL
   [: TWO-UNITS ;] E-NFEED-STATE TTHROWSQ
   s" a hexadecimal literal is refused, never recorded as zero" T-LABEL
   [: HEX ;] E-NFEED-LITERAL TTHROWSQ
   s" a float literal is refused: this stage has no kind for it" T-LABEL
   [: REAL ;] E-NFEED-KIND TTHROWSQ
   s" a definition longer than the unit's text buffer is refused" T-LABEL
   [: BIGTEXT ;] E-NFEED-TEXT TTHROWSQ
   s" a token event that lies about its offset is refused" T-LABEL
   [: LIAR ;] E-NFEED-SPAN TTHROWSQ
   s" a tape and a builder of two modules die on the owner check" T-LABEL
   [: XMOD ;] E-NTAPE-OWNER TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   GRID-A-CASE
   GRID-B-CASE
   GRID-C-CASE
   LIT-CASE
   REPEAT-CASE
   TRIM-CASE
   HIDE-COMMENT-CASE
   HIDE-STRING-CASE
   KEPT-CASE
   LITBYTE-CASE
   NAMEBYTE-CASE
   COMMENTBYTE-CASE
   SHAPEBYTE-CASE
   REFUSE-CASES
   RECOVER-CASE
   T-REPORT ;

;package

NFEED-TEST:RUN
