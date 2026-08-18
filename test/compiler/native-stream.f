\ native-stream.f - the migration entry that takes its definition from the input
\ stream: `NMIGRATE:NEXT : NAME ( .. ) body ;` written at top level instead of
\ inside a string. One concern: src/compiler/native/input.f and the NMIGRATE
\ entry over it.
\
\ WHAT THIS SUITE HAS TO SHOW.
\
\   1. That the DEFINITION IS THE ENGINE'S TO PARSE. Nothing here lexes anything
\      and neither does the entry: the whole tail of the stream is handed to the
\      engine and the engine's own reader says where the definition ended. So the
\      fixtures are written to fool a lexer that is not there - a `;` inside a
\      string body, twice; a quotation, whose closer is `;]`; a line comment
\      holding `;`, a `:` and the entry's own spelling; a stack comment holding
\      both. Each one is migrated and RUN, so a wrong extent is a wrong answer
\      and not just a wrong byte count.
\
\   2. That the interpreter is put back EXACTLY ONE BYTE PAST THE `;`, on the
\      failing path as well as the normal one. Every source below is built from a
\      head that ends with the definition's closing `;` and a tail the
\      interpreter has to reach afterwards, so the byte index is counted rather
\      than written down, and the fixture proves its own claim: the byte at that
\      index IS a `;`. NINP:RESUMED is then held against it. A landing one token
\      further along would still lex the same next token, which is why this is a
\      byte comparison and not "the tail ran" - the tail runs too, and both are
\      asserted.
\
\   3. That what the chain publishes is what NMIGRATE:DEFINE publishes. The
\      migrated words are checked with NPUB:REPUBLISHED?, so a NEXT that let the
\      engine's own compilation stand would fail here even though every answer
\      below would still be right.
\
\   4. That a refusal leaves the engine's word running, the stream past the
\      definition it consumed, and the recorder ready for the next one.
\
\ THE FILE'S OWN STREAM IS A CASE TOO. Everything above drives the entry through
\ `evaluate`, because that is the only way a test can state the bytes it is
\ measuring. The way tools/codegen-compare-migrated.f uses it - a definition at
\ the top level of a file being loaded - is the fixture at the head of the
\ private section below, migrated while this file loads.

require lib/test.f
require lib/string.f
require src/compiler/native/migrate.f
require src/compiler/native/input.f
require src/compiler/native/publish.f

\ The import the refusal fixture below reaches through. It is a package block of
\ its own because a package cannot be opened inside another one.
package NST-AWAY public 5 constant NST-K ;package

package NSTREAM-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite makes a stream of its own and compiles callers for words that
\ did not exist when the suite was compiled.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

0 constant GLOBAL-WID

\ ---- the entry through a FILE's own stream -----------------------------------
\ Compiled while this file loads, at the top level of the stream the loader is
\ reading, which is the position tools/codegen-compare-migrated.f writes its
\ definitions in. It is private to this package, so it also shows the definition
\ lands in the wordlist that is open where it is written.
NMIGRATE:NEXT : FILE-N ( n -- n ) dup + 1+ ;

\ ---- and a refusal in that same stream ---------------------------------------
\ THE CATCH HAS TO STAND INSIDE THE STREAM THAT COMPILED THE DEFINITION. A throw
\ that crosses an `evaluate` is recovered by the engine, which gives back the
\ definitions that evaluation made - so a refusal caught OUTSIDE the stream
\ leaves no word to ask about, and the case that asks whether the engine's word
\ survives has to be written here, in the file's own stream, exactly as a program
\ using the entry would meet it.
\
\ The body is refused for the reason test/compiler/native-migrate.f states: the
\ chain's resolver does not walk the used-publics leg, so a name reached through
\ `using` is compiled by the engine and declined by the chain. The rc lands in a
\ cell from the token AFTER the definition, which is also how the case knows the
\ interpreter came back to that token.
variable REFUSED-RC

: TRY-NEXT ( -- n )
   [: NMIGRATE:NEXT ;] catch ;

using NST-AWAY
TRY-NEXT : NST-FBAD ( n -- n ) NST-K + ;  REFUSED-RC !
;using

\ ---- what the fixtures store ------------------------------------------------
variable MARK-V
$80 constant KEEP-CAP
create KEEP-BUF KEEP-CAP allot
here CELL 1- and CELL swap - CELL 1- and allot
variable KEEP-U
PTR-VARIABLE LANDED-AT

public

\ Named by fixture tails, which run inside the stream the entry gave back. They
\ are the only reason this package has a public section.
: MARK! ( n -- )
   MARK-V ! ;

: KEEP$! ( ptr u8 n -- )
   {: a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   u KEEP-CAP > if E-STR-CAPACITY throw then
   a KEEP-BUF u BYTE-COPY
   u KEEP-U ! ;

\ A quotation is a value, and a body that answers one needs a caller that takes
\ it. This is that caller, and nothing else in the suite uses it.
: TAKE ( [ n -- n ] n -- n )
   swap execute ;

private

: MARK@ ( -- n )
   MARK-V @ ;

: KEEP$ ( -- ptr u8 n )
   KEEP-BUF KEEP-U @ ;

\ ---- one fixture source, built from its two halves ---------------------------
\ The HEAD ends with the definition's closing `;` and the TAIL is what the
\ interpreter must reach afterwards, so the index of that `;` is the head's
\ length less one - counted here rather than stated - and SEMI-CK proves the byte
\ there really is one. A head that does not end where it claims fails as a
\ fixture instead of passing as a measurement.
$200 constant SRC-CAP
create SRC-BUF SRC-CAP allot
here CELL 1- and CELL swap - CELL 1- and allot
variable SRC-U
variable SRC-SEMI

$3B constant SEMI-CH

: SRC-BUILD ( ptr u8 n ptr u8 n -- )
   {: ha:ptr hu:n ta:ptr tu:n :} \ typed-local-lint: allow-bare-local - ha and ta keep the ptr u8 byte-span role
   hu tu + SRC-CAP > if E-STR-CAPACITY throw then
   ha SRC-BUF hu BYTE-COPY
   ta SRC-BUF hu + tu BYTE-COPY
   hu tu + SRC-U !
   hu 1- SRC-SEMI ! ;

: SRC$ ( -- ptr u8 n )
   SRC-BUF SRC-U @ ;

: SRC-RUN ( -- )
   SRC$ EV ;

: SEMI-CK ( -- )
   SRC-BUF SRC-SEMI @ + c@ SEMI-CH T= ;

\ Where the entry put the interpreter back, as an index into the source this
\ suite built. One byte past the `;` and nothing else.
: LANDED ( -- n )
   NINP:RESUMED SRC-BUF - ;

: LANDED-CK ( -- )
   SEMI-CK
   LANDED SRC-SEMI @ 1+ T= ;

\ ---- the fixtures ------------------------------------------------------------
: PLAIN-HEAD ( -- ptr u8 n )
   s" NMIGRATE:NEXT : NST-PLAIN ( n -- n ) 1+ ;" ;

: PLAIN-TAIL ( -- ptr u8 n )
   s"  41 NST-PLAIN NSTREAM-TEST:MARK!" ;

\ The terminator inside a string body, twice, and once with no space around it.
\ A reader looking for `;` in the text lands inside the literal; the engine's
\ reader consumed the literal as one token and never saw them.
: STRING-HEAD ( -- ptr u8 n )
   S\" NMIGRATE:NEXT : NST-STR ( -- ptr u8 n ) s\" a ; b;c\" ;" ;

: STRING-TAIL ( -- ptr u8 n )
   s"  NST-STR NSTREAM-TEST:KEEP$!" ;

\ The quotation's closer IS the definition's terminator with a bracket after it.
: QUOT-HEAD ( -- ptr u8 n )
   s" NMIGRATE:NEXT : NST-QUOT ( -- [ n -- n ] ) [: 1 + ;] ;" ;

: QUOT-TAIL ( -- ptr u8 n )
   s"  NST-QUOT 4 NSTREAM-TEST:TAKE NSTREAM-TEST:MARK!" ;

\ A line comment carrying the terminator, a colon and the entry's own spelling,
\ and a stack comment carrying all three.
: COMMENT-HEAD ( -- ptr u8 n )
   S\" NMIGRATE:NEXT : NST-CMT ( n -- n ) \\ doubles: NMIGRATE:NEXT ; or not\n   ( a ; b : NMIGRATE:NEXT ) dup + ;" ;

: COMMENT-TAIL ( -- ptr u8 n )
   s"  21 NST-CMT NSTREAM-TEST:MARK!" ;

\ Two definitions, one entry each, nothing between them: the second unit opens
\ only if the first one closed.
: PAIR-HEAD ( -- ptr u8 n )
   s" NMIGRATE:NEXT : NST-B1 ( n -- n ) 1+ ; NMIGRATE:NEXT : NST-B2 ( n -- n ) 2 * ;" ;

: PAIR-TAIL ( -- ptr u8 n )
   s"  20 NST-B2 NST-B1 NSTREAM-TEST:MARK!" ;

\ A body the CHAIN refuses and the ENGINE compiles: a double crossing into a
\ memory cell, which the elaborator has no placement for. Refused in a stream of
\ this suite's own making, which is what makes the landing measurable; what a
\ refusal leaves behind in the stream a PROGRAM writes is the file case above.
: REFUSED-HEAD ( -- ptr u8 n )
   s" NMIGRATE:NEXT : NST-BAD ( r ptr a -- ) {: v:r b:ptr :} v 1.0 f+ b ! ;" ;

: REFUSED-TAIL ( -- ptr u8 n )
   s"  1 NSTREAM-TEST:MARK!" ;

\ After the refusal, in a stream of its own.
: AFTER-HEAD ( -- ptr u8 n )
   s" NMIGRATE:NEXT : NST-AFTER ( n -- n ) 3 * ;" ;

: AFTER-TAIL ( -- ptr u8 n )
   s"  5 NST-AFTER NSTREAM-TEST:MARK!" ;

\ No definition after the entry at all.
: NO-DEF-HEAD ( -- ptr u8 n )
   s" NMIGRATE:NEXT 7 NSTREAM-TEST:MARK! ;" ;

: NO-DEF-TAIL ( -- ptr u8 n )
   s"  " ;

\ ---- the cases ---------------------------------------------------------------
: FILE-CASE ( -- )
   s" the entry migrates the definition written after it in a FILE's stream" T-LABEL
   4 FILE-N 9 T=
   -3 FILE-N -5 T=

   s" and what runs is the chain's code, not the engine's" T-LABEL
   s" FILE-N" NMIGRATE:WID NPUB:REPUBLISHED? TTRUE ;

: FILE-REFUSED-CASE ( -- )
   \ The cell is written by the token AFTER the definition, so holding the
   \ dialect's code is also the statement that the stream came back to that
   \ token. The stronger half of it is that this file loaded at all: a stream
   \ that did not come back reads the definition a second time and the load dies
   \ on the duplicate.
   s" a body the chain declines is refused by the dialect's own name" T-LABEL
   REFUSED-RC @ E-HIR-UNMODELED T=

   s" the word the engine published is still there and still runs" T-LABEL
   37 NST-FBAD 42 T=

   s" and the publication seam never logged it" T-LABEL
   s" NST-FBAD" NMIGRATE:WID NPUB:REPUBLISHED? TFALSE ;

: PLAIN-CASE ( -- )
   0 MARK-V !
   PLAIN-HEAD PLAIN-TAIL SRC-BUILD
   SRC-RUN

   s" the definition after the entry is compiled and published by the chain" T-LABEL
   NMIGRATE:NAME$ s" NST-PLAIN" T$=
   s" NST-PLAIN" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" the interpreter carried on with the token after the definition" T-LABEL
   MARK@ 42 T=

   s" and it was put back exactly one byte past the `;`" T-LABEL
   LANDED-CK ;

: STRING-CASE ( -- )
   0 KEEP-U !
   STRING-HEAD STRING-TAIL SRC-BUILD
   SRC-RUN

   s" a `;` inside a string body is the string's, not the definition's" T-LABEL
   KEEP$ s" a ; b;c" T$=
   s" NST-STR" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" and the landing is the definition's own terminator" T-LABEL
   LANDED-CK ;

: QUOT-CASE ( -- )
   0 MARK-V !
   QUOT-HEAD QUOT-TAIL SRC-BUILD
   SRC-RUN

   s" a quotation's `;]` does not end the definition" T-LABEL
   MARK@ 5 T=
   s" NST-QUOT" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" and the landing is the definition's own terminator" T-LABEL
   LANDED-CK ;

: COMMENT-CASE ( -- )
   0 MARK-V !
   COMMENT-HEAD COMMENT-TAIL SRC-BUILD
   SRC-RUN

   s" a `;`, a `:` and the entry's own spelling inside comments are comments" T-LABEL
   MARK@ 42 T=
   s" NST-CMT" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" and the landing is the definition's own terminator" T-LABEL
   LANDED-CK ;

: PAIR-CASE ( -- )
   0 MARK-V !
   PAIR-HEAD PAIR-TAIL SRC-BUILD
   SRC-RUN

   s" two entries in one stream migrate two definitions" T-LABEL
   MARK@ 41 T=
   s" NST-B1" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   s" NST-B2" GLOBAL-WID NPUB:REPUBLISHED? TTRUE

   s" and the second one's landing is the second `;`" T-LABEL
   NMIGRATE:NAME$ s" NST-B2" T$=
   LANDED-CK ;

\ The refusal, and the three things it must leave behind.
: REFUSED-RUN ( -- )
   REFUSED-HEAD REFUSED-TAIL SRC-BUILD
   SRC-RUN ;

: REFUSED-CASE ( -- )
   0 MARK-V !
   s" a body the chain cannot compile is refused by the dialect's own name" T-LABEL
   [: REFUSED-RUN ;] E-NELAB-TYPE TTHROWSQ

   s" the stream was given back one byte past the `;` on the failing path too" T-LABEL
   LANDED-CK

   s" the throw left the rest of that stream unread" T-LABEL
   MARK@ 0 T=

   s" and the recorder takes the NEXT definition from a stream as usual" T-LABEL
   0 MARK-V !
   AFTER-HEAD AFTER-TAIL SRC-BUILD
   SRC-RUN
   MARK@ 15 T=
   s" NST-AFTER" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   LANDED-CK ;

: NO-DEF-RUN ( -- )
   NO-DEF-HEAD NO-DEF-TAIL SRC-BUILD
   SRC-RUN ;

: NO-DEF-CASE ( -- )
   0 MARK-V !
   s" an entry that is not followed by a definition is refused" T-LABEL
   [: NO-DEF-RUN ;] E-NINP-DEF TTHROWSQ

   s" and the refusal read no token of the stream it refused" T-LABEL
   MARK@ 0 T= ;

\ ---- the stream module's own state machine ------------------------------------
\ Two migrations reading one stream is the refusal NINP owns. CLOSE is asked of
\ EVERY recorded definition - src/compiler/native/feed.f calls it for the string
\ entry's units too - so an unarmed close cannot be a refusal; the case below is
\ that it says nothing at all, measured on the landing it would have written.
: ARM-TWICE ( -- )
   NINP:ARM
   [: NINP:ARM ;] E-NINP-STATE TTHROWSQ
   NINP:RELEASE ;

: STATE-CASE ( -- )
   s" a second migration arming one stream is refused" T-LABEL
   ARM-TWICE

   s" an unarmed stream is not cut by the close every unit reaches" T-LABEL
   NINP:RESUMED LANDED-AT !
   s" : NST-QUIET ( n -- n ) 1+ ;" NMIGRATE:DEFINE
   s" NST-QUIET" GLOBAL-WID NPUB:REPUBLISHED? TTRUE
   NINP:RESUMED LANDED-AT @ = TTRUE ;

public

: RUN ( -- )
   T-RESET
   FILE-CASE
   FILE-REFUSED-CASE
   PLAIN-CASE
   STRING-CASE
   QUOT-CASE
   COMMENT-CASE
   PAIR-CASE
   REFUSED-CASE
   NO-DEF-CASE
   STATE-CASE
   T-REPORT ;

;package

NSTREAM-TEST:RUN
