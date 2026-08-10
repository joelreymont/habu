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

\ A double is one unboxed cell holding its own IEEE754 bits, so a test that
\ wants to compare a recorded literal with the engine's own literal retypes the
\ engine's and compares cells. It is the CAST: form the checker certifies.
CAST: REAL-CELL ( r -- n ) ;

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

\ ---- the token a keyword reads for itself ------------------------------------
\ `is NAME` does not read NAME through the scan loop: the judgement reads it,
\ out of the middle of the loop's turn, and walks the reader's cursor past it.
\ So the loop's one report per turn describes `is` and the loop resumes AFTER
\ the name - which used to leave the tape without a row for a token the reader
\ really had consumed. Measured before the repair: this very definition recorded
\ five rows ending at `is`.
\
\ THE ROW HAS TO BE THERE FOR THE TAPE TO MEAN WHAT IT SAYS. src/compiler/native/
\ tape.f is "the exact token stream the compiler consumed" and the native chain
\ has no other way to learn which deferred word a definition binds: a second
\ lexer over the kept text is precisely what the tape exists to make
\ unnecessary. So the row is asserted in its place - straight after `is`, in
\ consumption order - with its own kind, mode and span.
\ ---- reading a row that may not be there --------------------------------------
\ The claim these fixtures make is "there is a row here and it says this", and a
\ producer that stopped making the row would make the readers above throw on the
\ index instead of answering. A throw out of a fixture ends the suite with a
\ code and names no case, so the three readers below answer FALSE - and -1 for a
\ length - for a row the tape does not have, and the case reports which claim
\ broke.
: ROW-SPELL? ( n n ptr u8 n -- bool )
   {: slot:n i:n a:ptr u:n :}
   i 0 < i slot TOKENS >= or if false exit then
   slot i a u SPELL-IS? ;

: ROW-KIND? ( n n NTAPE:kind -- bool )
   {: slot:n i:n k:NTAPE:kind :}
   i 0 < i slot TOKENS >= or if false exit then
   slot i k KIND-IS? ;

: ROW-MODE? ( n n NTAPE:mode -- bool )
   {: slot:n i:n m:NTAPE:mode :}
   i 0 < i slot TOKENS >= or if false exit then
   slot i m MODE-IS? ;

: ROW-SPAN-LEN ( n n -- n )
   {: slot:n i:n :}
   i 0 < i slot TOKENS >= or if -1 exit then
   slot i SPAN-LEN ;

\ The deferred word and the body the three fixtures below bind to it, declared
\ once. A `defer` may not be declared twice - the checker refuses the second as
\ a duplicate definition - so the three cases share one declaration rather than
\ each making its own.
variable SW-READY

: SWALLOW-PREP ( -- )
   SW-READY @ 0<> if exit then
   1 SW-READY !
   s" defer NF-SW-HOOK ( n -- n )" EV
   s" : NF-SW-IMPL ( n -- n ) 1 + ;" EV ;

: SWALLOW-BODY ( IR-CTX:ctx -- n bool bool bool bool n )
   {: c:IR-CTX:ctx :}
   SWALLOW-PREP
   c s" : NF-SWALLOW ( -- ) [: NF-SW-IMPL ;] is NF-SW-HOOK ;" 0 REC
   0 TOKENS
   0 4 s" is" ROW-SPELL?
   0 5 s" NF-SW-HOOK" ROW-SPELL?
   0 5 NTAPE-KIND:NAME ROW-KIND?
   0 5 NTAPE-MODE:COMPILING ROW-MODE?
   0 5 ROW-SPAN-LEN ;

: SWALLOW-CASE ( -- )
   s" the name `is` reads for itself is a row of its own, in order" T-LABEL
   BND [: SWALLOW-BODY ;] IR-CTX:WITH-CONTEXT
   10 T= TTRUE TTRUE TTRUE TTRUE 6 T= ;

\ AND THE ROW IS THE JUDGEMENT'S, NOT A SPELLING'S. This fixture puts a string
\ literal whose whole body is `is` in front of the real one. A producer that
\ flushed a target row after any row spelled `is` would give the literal one
\ too, and the count would be nine; what decides it here is whether the
\ judgement really consumed a token that turn, and a string literal consumes
\ none. The parenthesised copy is asserted as well: the engine drops it while it
\ reconstructs the body, so it is not even in the text the reader sees, which is
\ a stronger statement than "the reader steps over it".
: SWALLOW-HIDE-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   SWALLOW-PREP
   c S\" : NF-SWHID ( -- ) ( is NF-SW-HOOK ) s\" is\" 2drop [: NF-SW-IMPL ;] is NF-SW-HOOK ;"
   0 REC
   0 TOKENS
   0 1 NTAPE-KIND:STRING-LITERAL ROW-KIND?
   0 1 s" is" ROW-SPELL?
   0 2 s" 2drop" ROW-SPELL?
   0 6 s" is" ROW-SPELL?
   0 7 s" NF-SW-HOOK" ROW-SPELL? ;

: SWALLOW-HIDE-CASE ( -- )
   s" a string literal spelled `is` reads no name after it" T-LABEL
   BND [: SWALLOW-HIDE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 8 T= ;

\ ---- the target `[']` reads, which never reaches this reader at all -----------
\ `['] W` reads a token for itself the same way `is` does, and the producer
\ reports it the same way - but no tape can hold it, because the ENGINE drops it
\ while it reconstructs the body: src/habu/habu2.f C-BTICK consumes the target
\ with no body capture, which src/core/checker.f's own note on BTICK-CAND?
\ records. So the text this reader is handed ends at the `[']`, and the missing
\ row is one stage further up than anything the checker can repair.
\
\ IT IS PINNED HERE RATHER THAN LEFT UNSAID because the count is what a reader
\ would otherwise take for a hole in the repair above. Dot
\ habu-capture-a-tick-f2bf9d91 carries the engine half; when it lands this
\ case is what moves.
: TICK-BODY ( IR-CTX:ctx -- n bool )
   {: c:IR-CTX:ctx :}
   SWALLOW-PREP
   c s" : NF-TICKED ( -- n ) ['] NF-SW-IMPL ;" 0 REC
   0 TOKENS
   0 s" NF-SW-IMPL" ANY-SPELL? ;

: TICK-CASE ( -- )
   s" the engine drops a tick's target before the reader can see it" T-LABEL
   BND [: TICK-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE 2 T= ;

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

\ The same for a string payload, and this fixture is hostile in both directions
\ at once. The payload holds two spellings that would be ordinary tokens
\ anywhere else - a word and a number - and NEITHER may name a row of its own:
\ the reader spends those bytes rather than tokenising them, which is exactly
\ what stops a quoted word from being read as code. What the tape says instead
\ is what the literal IS - one row, of the string kind, spelled the WHOLE body -
\ and the opener does not name a row either, because the opener is how the body
\ was written rather than what it is.
: HIDE-STRING-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c S\" : NF-HIDES ( -- n ) s\" zdup z77\" 2drop 5 ;" 0 REC
   0 TOKENS
   0 s" zdup" ANY-SPELL?
   0 s" z77" ANY-SPELL?
   0 S\" s\q" ANY-SPELL?
   0 1 NTAPE-KIND:STRING-LITERAL KIND-IS?
   0 1 s" zdup z77" SPELL-IS? ;

: HIDE-STRING-CASE ( -- )
   s" a spelling hidden in a string payload never becomes a row" T-LABEL
   BND [: HIDE-STRING-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TFALSE TFALSE TFALSE 4 T= ;

\ ---- the body a string literal really carried ---------------------------------
\ VERBATIM, AND THAT IS THE WHOLE POINT. The reconstructed definition the checker
\ reads has had its backslash comments removed and its runs of whitespace
\ collapsed (TRIM-CASE above), and NONE of that reaches inside a literal: the
\ engine appends a payload to the body capture as a raw byte run. So a body with
\ two spaces in it keeps two spaces, and a body holding a definition closer, a
\ colon and a comment opener keeps all three as ordinary bytes. A tape that
\ collapsed or re-lexed any of it would compile a string that is not the string
\ the programmer wrote, and nothing downstream could tell.
: VERBATIM-BODY ( IR-CTX:ctx -- n bool bool )
   {: c:IR-CTX:ctx :}
   c S\" : NF-VERB ( -- n ) s\" a  b ; : ( x\" 2drop 5 ;" 0 REC
   0 TOKENS
   0 1 s" a  b ; : ( x" SPELL-IS?
   0 1 NTAPE-KIND:STRING-LITERAL KIND-IS? ;

: VERBATIM-CASE ( -- )
   s" a string literal's body is recorded verbatim" T-LABEL
   BND [: VERBATIM-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 4 T= ;

\ AN ESCAPED LITERAL'S BODY IS WHAT ITS ESCAPES DECODE TO. The checker owns the
\ decoding, so the row's spelling is three bytes here and not the eight the
\ literal was written as - and the SPAN is still the eight, because the span says
\ where the literal was read from while the spelling says what it is. Both halves
\ are asserted, because a decoder that also shortened the span would leave every
\ later diagnostic pointing at the wrong bytes.
\
\ THE TABLE IS EXERCISED RATHER THAN SAMPLED: a named escape, a hex escape and a
\ quote escape in one body. \q is a double quote that does NOT close the literal,
\ which is the case a decoder that merely scanned for the closing quote would get
\ wrong.
: ESCAPE-BODY ( IR-CTX:ctx -- n n bool )
   {: c:IR-CTX:ctx :}
   c S\" : NF-ESC ( -- n ) s\\\" a\\tb\\x41\\qc\" 2drop 5 ;" 0 REC
   0 TOKENS
   0 1 SPAN-LEN
   0 1 S\" a\tbA\qc" SPELL-IS? ;

: ESCAPE-CASE ( -- )
   s" an escaped literal records its decoded body and its raw span" T-LABEL
   BND [: ESCAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 11 T= 4 T= ;

\ THE EMPTY STRING IS A STRING. `s" "` parses as a literal with no bytes at all,
\ so the row is present, of the string kind, and spelled nothing - and the feed's
\ span check has to admit a zero length where its byte check refuses one.
: EMPTY-BODY ( IR-CTX:ctx -- n bool bool )
   {: c:IR-CTX:ctx :}
   c S\" : NF-EMPTY ( -- n ) s\" \" 2drop 5 ;" 0 REC
   0 TOKENS
   0 1 NTAPE-KIND:STRING-LITERAL KIND-IS?
   0 1 s" " SPELL-IS? ;

: EMPTY-CASE ( -- )
   s" the empty string literal is a string row with no bytes" T-LABEL
   BND [: EMPTY-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 4 T= ;

\ TWO EQUAL BODIES ARE ONE SYMBOL AND TWO ROWS, which is the same rule the
\ REPEAT-CASE above pins for names: the module's interner deduplicates the bytes,
\ the tape does not deduplicate the token. It is asserted here because the
\ elaborator interns a literal's ADDRESS off this symbol, so two equal literals
\ sharing one symbol is what makes them share one address.
: SAME-BODY ( IR-CTX:ctx -- n bool bool )
   {: c:IR-CTX:ctx :}
   c S\" : NF-SAME ( -- n ) s\" ab\" 2drop s\" ab\" 2drop 5 ;" 0 REC
   0 TOKENS
   0 1 LOCAL-SPELL  0 3 LOCAL-SPELL  =
   0 1 SPAN-START   0 3 SPAN-START   <> ;

: SAME-CASE ( -- )
   s" two equal string bodies are one symbol and two rows" T-LABEL
   BND [: SAME-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 6 T= ;

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

\ A float literal records the CELL the engine's own reader would have pushed for
\ that spelling. The row's kind says a real literal and its value is that cell,
\ so the comparison below is against the engine's literal and not against a
\ number this test worked out for itself.
: REAL-BODY ( IR-CTX:ctx -- bool bool n )
   {: c:IR-CTX:ctx :}
   c s" : NF-REAL ( -- r ) 1.5 ;" 0 REC
   0 1 NTAPE-KIND:REAL-LITERAL KIND-IS?
   0 1 s" 1.5" SPELL-IS?
   0 1 LIT ;

: REAL-CASE ( -- )
   s" a float literal is recorded as the cell the engine reads it as" T-LABEL
   BND [: REAL-BODY ;] IR-CTX:WITH-CONTEXT
   1.5 REAL-CELL T=
   TTRUE TTRUE ;

\ The sign of a zero is a bit of the cell, and a recorded literal keeps it: -0.0
\ and 0.0 are equal numbers in two different cells, so a reader that folded one
\ into the other would record the same row for both.
: NEG-ZERO-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c s" : NF-NZERO ( -- r ) -0.0 ;" 0 REC
   c s" : NF-PZERO ( -- r ) 0.0 ;" 1 REC
   0 1 LIT
   1 1 LIT ;

: NEG-ZERO-CASE ( -- )
   s" a recorded float literal keeps the sign of a zero" T-LABEL
   BND [: NEG-ZERO-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= -0.0 REAL-CELL T= ;

\ The three spellings the survey at the head of tools/codegen-compare-corpus3.f
\ measures as the engine's own answers, including the two where the engine's
\ route lands one bit off the nearest double and the one where the fractional
\ accumulator wraps into a negative cell. A reader that used the stdlib's float
\ parser instead would agree on ordinary literals and disagree on exactly these,
\ so this is the case that says the two routes are one route.
: AWKWARD-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c s" : NF-R1 ( -- r ) 1.9482199351819093 ;" 0 REC
   0 1 LIT
   c s" : NF-R2 ( -- r ) 0.11471049746507529 ;" 1 REC
   1 1 LIT
   c s" : NF-R3 ( -- r ) 0.1234567890123456789 ;" 0 REC
   0 1 LIT ;

: AWKWARD-CASE ( -- )
   s" a float literal the engine reads inexactly is recorded as the engine reads it" T-LABEL
   BND [: AWKWARD-BODY ;] IR-CTX:WITH-CONTEXT
   0.1234567890123456789 REAL-CELL T=
   0.11471049746507529 REAL-CELL T=
   1.9482199351819093 REAL-CELL T= ;

\ What the reader behind the recording refuses. Every one of these is a spelling
\ the engine's own float path cannot read either, so a tape that accepted one
\ would carry a value no interpreted literal has.
: READ-NONE? ( ptr u8 n -- bool )
   NREAL:READ MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH ;

: READ-SOME ( ptr u8 n -- n )
   NREAL:READ MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

: READER-CASE ( -- )
   s" the literal reader declines every spelling the engine declines" T-LABEL
   s" 5." READ-NONE? TTRUE
   s" 12" READ-NONE? TTRUE
   s" 1.2.3" READ-NONE? TTRUE
   s" 1.5e3" READ-NONE? TTRUE
   s" " READ-NONE? TTRUE
   s" a dot-leading spelling is a float literal, as the engine has it" T-LABEL
   s" .5" READ-SOME .5 REAL-CELL T=
   s" -.5" READ-SOME -.5 REAL-CELL T= ;

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
   SWALLOW-CASE
   SWALLOW-HIDE-CASE
   TICK-CASE
   HIDE-COMMENT-CASE
   HIDE-STRING-CASE
   VERBATIM-CASE
   ESCAPE-CASE
   EMPTY-CASE
   SAME-CASE
   KEPT-CASE
   LITBYTE-CASE
   NAMEBYTE-CASE
   COMMENTBYTE-CASE
   SHAPEBYTE-CASE
   REAL-CASE
   NEG-ZERO-CASE
   AWKWARD-CASE
   READER-CASE
   REFUSE-CASES
   RECOVER-CASE
   T-REPORT ;

;package

NFEED-TEST:RUN
