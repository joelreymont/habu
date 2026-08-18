\ native-string.f - a string literal, compiled by the native chain and run.
\ One concern: that `s" ..."` in a colon body becomes a routine of the chain's
\ making which pushes the right bytes and the right length.
\
\ WHAT THIS SUITE HAS TO SHOW.
\
\   1. That the chain compiles it AT ALL, through the production entry. Every
\      case here goes through src/compiler/native/migrate.f DEFINE: the engine
\      compiles the definition, the checker's own reader fills the tape while it
\      certifies the body, and the chain recompiles that tape and republishes the
\      record. Before this tranche each of these was E-HIR-UNMODELED on the token
\      `s"`.
\   2. That the bytes survive the round trip EXACTLY. The published word is then
\      entered and what it pushes is compared with the string the source wrote -
\      including bodies built to fool a reader that re-lexed them, and bodies
\      whose escapes have to decode.
\   3. That equal bodies share one address and different bodies do not, because
\      that is what makes a re-elaborated definition cost nothing and what keeps
\      the store bounded.
\   4. That the store's ceiling is a named refusal rather than a reused address.
\
\ WHY THE ANSWERS ARE COMPARED BY RUNNING THE WORD. A test that read the tape or
\ the module would be describing what the compiler intended. Entering the
\ published record is the only question whose answer is the code that actually
\ runs, and the record has been retargeted at the chain's emission by then -
\ test/compiler/native-migrate.f pins that retarget, so it is not restated here.

require lib/test.f
require lib/string.f
require src/compiler/native/migrate.f
require src/compiler/native/string.f

package NSTRING-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite compiles a caller for a word that did not exist when the
\ suite was compiled. The bool form is separate because a flag is what most of
\ these questions answer with.
TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

TRUSTED: EV-B ( ptr u8 n -- bool )
   evaluate ;

\ ---- the words the chain compiles --------------------------------------------
\ Each is migrated through the production entry, which reads what the definition
\ takes and leaves off the checker's certificate: `( -- ptr u8 n )` is TWO, an
\ address and a length, and nothing here states it.
: DEF ( ptr u8 n -- )
   NMIGRATE:DEFINE ;

: PLAIN ( -- )
   S\" : NST-PLAIN ( -- ptr u8 n ) s\" hi\" ;" DEF ;

: EMPTY ( -- )
   S\" : NST-EMPTY ( -- ptr u8 n ) s\" \" ;" DEF ;

\ A body built to fool a reader that re-lexed it: two spaces that must not
\ collapse, a definition closer, a colon and a comment opener that must not
\ become syntax, and a trailing word that must not become a token.
: HOSTILE ( -- )
   S\" : NST-HOSTILE ( -- ptr u8 n ) s\" a  b ; : ( x\" ;" DEF ;

\ A named escape, a hex escape and a quote escape in one body. The quote escape
\ is the one a decoder that merely scanned for the closing quote would get wrong.
: ESCAPED ( -- )
   S\" : NST-ESCAPED ( -- ptr u8 n ) s\\\" a\\tb\\x41\\qc\" ;" DEF ;

\ Two sites in ONE definition, writing the same body, so the addresses they push
\ can be compared against each other.
: TWICE ( -- )
   S\" : NST-TWICE ( -- ptr u8 n ptr u8 n ) s\" dup\" s\" dup\" ;" DEF ;

\ And a second definition writing that same body, so the sharing can be shown to
\ cross a definition boundary and not only a site boundary.
: SHARED ( -- )
   S\" : NST-SHARED ( -- ptr u8 n ) s\" dup\" ;" DEF ;

\ A body nothing else in this suite writes, for the counting cases, and a second
\ definition writing exactly it.
: LONE ( -- )
   S\" : NST-LONE ( -- ptr u8 n ) s\" lone-body\" ;" DEF ;

: LONE-AGAIN ( -- )
   S\" : NST-LONE2 ( -- ptr u8 n ) s\" lone-body\" ;" DEF ;

\ ---- what the published words answer -----------------------------------------
: ROUND-TRIP-CASE ( -- )
   PLAIN
   s" a string literal compiles through the chain and pushes its bytes" T-LABEL
   S\" NST-PLAIN s\q hi\q STR=" EV-B TTRUE
   s" NST-PLAIN nip" EV-N 2 T=

   EMPTY
   s" the empty string literal pushes a valid address and zero" T-LABEL
   s" NST-EMPTY nip" EV-N 0 T=
   s" NST-EMPTY drop 0 >" EV-B TTRUE

   HOSTILE
   s" a body that would re-lex as syntax survives verbatim" T-LABEL
   S\" NST-HOSTILE s\q a  b ; : ( x\q STR=" EV-B TTRUE
   s" NST-HOSTILE nip" EV-N 12 T=

   ESCAPED
   s" an escaped body arrives decoded, not as the text it was written as" T-LABEL
   S\" NST-ESCAPED S\\\q a\\tbA\\qc\q STR=" EV-B TTRUE
   s" NST-ESCAPED nip" EV-N 6 T= ;

\ ---- one body, one address ----------------------------------------------------
\ THE SHARING IS THE POINT AND NOT AN ECONOMY. A store that answered a fresh
\ address for equal bytes would leak a copy every time the pipeline re-elaborated
\ a definition it had refused, so this is the property that stands in for "a
\ refusal moves nothing" over the arena.
: SHARING-CASE ( -- )
   TWICE
   s" two sites writing one body push one address" T-LABEL
   s" NST-TWICE drop swap drop =" EV-B TTRUE

   SHARED
   s" a second definition writing that body pushes the same address" T-LABEL
   s" NST-SHARED drop NST-TWICE drop nip drop =" EV-B TTRUE

   s" and a different body pushes a different address" T-LABEL
   s" NST-SHARED drop NST-PLAIN drop <>" EV-B TTRUE ;

\ ---- interning the same bytes twice costs nothing ------------------------------
\ WHY A SECOND MIGRATION IS A SECOND NAME AND NOT THE SAME ONE. Re-evaluating a
\ definition's source is a duplicate definition and the engine refuses it, so the
\ retry this property exists for - the pipeline re-elaborating a tape it already
\ elaborated - cannot be staged that way from outside. What CAN be staged is the
\ question underneath it: a body the store has already seen costs nothing the
\ next time it is compiled, wherever it is compiled from.
: INTERN-IDEMPOTENT-CASE ( -- )
   LONE
   s" a body already interned adds no row and no bytes" T-LABEL
   NSTR:COUNT {: c0:n :}
   NSTR:BYTES {: b0:n :}
   s" lone-body" NSTR:INTERN {: a1:n :}
   s" lone-body" NSTR:INTERN {: a2:n :}
   a1 a2 T=
   NSTR:COUNT c0 T=
   NSTR:BYTES b0 T=

   s" and a second definition writing that body adds none either" T-LABEL
   LONE-AGAIN
   NSTR:COUNT c0 T=
   NSTR:BYTES b0 T=
   S\" NST-LONE2 s\q lone-body\q STR=" EV-B TTRUE
   s" NST-LONE drop NST-LONE2 drop =" EV-B TTRUE ;

\ ---- what a literal costs in code bytes -----------------------------------------
\ THE MEASUREMENT THE DESIGN TURNS ON, taken through the publication seam's own
\ two readers: OLD-LEN is the code the ENGINE compiled for that definition and
\ NEW-LEN is the code the chain published in its place, so one migration answers
\ both columns for one body and nothing has to be lined up by hand.
\
\ WHAT THE OLD EMITTER SPENDS. src/habu/habu2.f C-SDQ copies the payload INLINE
\ into the compiled word, jumps over it, and addresses it pc-relatively, so its
\ cost grows with the string: measured here, a two-byte body costs 48 bytes and a
\ thirty-two-byte body costs 76, and the difference is exactly the payload
\ growth, 32 bytes rounded to four against 2 rounded to four.
\
\ WHAT THE CHAIN SPENDS. Two constants and no payload at all, because the bytes
\ are in DATA space: measured here, 28 bytes for BOTH bodies. That is the claim
\ this case exists to keep honest, and it is asserted as an EQUALITY rather than
\ as a number, because it is the independence that matters - a change that put a
\ literal's bytes back into the code region would move the two apart whatever the
\ absolute figures became.
\
\ WHICH PINS ARE ABSOLUTE AND WHICH ARE RELATIONS. The old emitter's layout does
\ not depend on where anything is mapped, so its two lengths are pinned as
\ numbers. The chain's does: an address literal is a move-wide chain, and how
\ many halfwords it needs depends on the arena's address, which is a fact about
\ the host's DATA base rather than about this compiler. So the chain's side is
\ pinned as the two relations that carry the claim.
: COST-PAIR ( -- )
   S\" : NST-COST-SHORT ( -- ptr u8 n ) s\" hi\" ;" DEF
   S\" : NST-COST-LONG ( -- ptr u8 n ) s\" 12345678901234567890123456789012\" ;" DEF ;

: OLD ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u 0 NPUB:OLD-LEN ;

: NEW ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u 0 NPUB:NEW-LEN ;

: BYTE-COST-CASE ( -- )
   COST-PAIR
   s" the old emitter's code grows with the string, by the payload" T-LABEL
   s" NST-COST-SHORT" OLD 48 T=
   s" NST-COST-LONG" OLD 76 T=

   s" the chain's code does not grow with the string at all" T-LABEL
   s" NST-COST-LONG" NEW  s" NST-COST-SHORT" NEW T=

   s" and it is smaller than the old emitter's for both" T-LABEL
   s" NST-COST-SHORT" NEW  s" NST-COST-SHORT" OLD < TTRUE
   s" NST-COST-LONG" NEW  s" NST-COST-LONG" OLD < TTRUE ;

\ ---- the store's ceiling ------------------------------------------------------
\ THIS CASE RUNS LAST AND EXHAUSTS THE STORE, so nothing after it can intern.
\ What it proves is that a body the store cannot take is refused by name: the
\ addresses already answered are compiled into published routines, so handing the
\ same address to different bytes is the one outcome that must be impossible.
\ The loop is bounded by a number larger than either ceiling rather than by
\ either ceiling itself, so it does not restate the constants it is testing.
create FILL-BUF 64 allot

: FILL-BODY ( n -- ptr u8 n ) {: k:n :}
   8 0 ?do
      k i 8 * rshift $FF and  FILL-BUF i +  c!
   loop
   FILL-BUF 64 ;

: FILL-ONE ( n -- ) {: k:n :}
   k FILL-BODY NSTR:INTERN drop ;

: FILL ( -- )
   20000 0 ?do i FILL-ONE loop ;

: CAP-CASE ( -- )
   s" a body the store cannot hold is refused by name" T-LABEL
   [: FILL ;] E-NSTR-CAP TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   ROUND-TRIP-CASE
   SHARING-CASE
   INTERN-IDEMPOTENT-CASE
   BYTE-COST-CASE
   CAP-CASE
   T-REPORT ;

;package

NSTRING-TEST:RUN
