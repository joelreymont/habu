\ native-string.f - a string literal, compiled by the native chain and run.

require lib/test.f
require lib/string.f
require src/compiler/native/compiler.f
require src/compiler/native/string.f
require src/habu/aot-arm.f

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
\ Each is compiled through the production entry, which reads what the definition
\ takes and leaves off the checker's certificate: `( -- ptr u8 n )` is TWO, an
\ address and a length, and nothing here states it.
TRUSTED: DEF ( ptr u8 n -- )
   evaluate ;

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
\ Re-evaluating a definition's source would be a duplicate definition. The
\ underlying property is that interning bytes already present costs nothing.
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

\ A failed evaluate rewinds DATA but keeps the compiler's literal table.
\ Reclaim that DATA and verify the cached bytes still belong to its pool.
public
variable SAVED-LITERAL
private
create ROLLBACK-BYTES 101 c, 112 c, 104 c, 101 c, 109 c, 101 c, 114 c, 97 c, 108 c,
TRUSTED: PTR>N ( ptr a -- n ) ;
TRUSTED: N>BYTES ( n -- ptr u8 ) ;

: FAILED-INTERN ( -- )
   S\" s\q ephemeral\q NSTR:INTERN NSTRING-TEST:SAVED-LITERAL ! 77 throw" DEF ;

: ROLLBACK-CASE ( -- )
   s" literals survive DATA rollback after failed evaluation" T-LABEL
   here PTR>N {: before:n :}
   [: FAILED-INTERN ;] 77 TTHROWSQ
   SAVED-LITERAL @ before < TTRUE
   here {: reclaimed:ptr :}
   128 allot
   128 0 ?do 88 reclaimed i + c! loop
   S\" : NST-ROLLBACK ( -- ptr u8 n ) s\q ephemeral\q ;" DEF
   SAVED-LITERAL @ N>BYTES 9 ROLLBACK-BYTES 9 STR= TTRUE
   s" NST-ROLLBACK drop" EV-N SAVED-LITERAL @ T= ;

: WINDOW-CASE ( -- )
   s" lone-body" NSTR:INTERN {: old:n :}
   data-base AOT-WINDOW:B0-CELL + @ {: b0:n :}
   data-base AOT-WINDOW:D0-CELL + @ {: d0:n :}
   AOT-ARM:WINDOW-OPEN
   NSTR:WINDOW-OPEN
   s" lone-body" NSTR:INTERN {: fresh:n :}
   s" lone-body" NSTR:INTERN {: again:n :}
   b0 d0 AOT-ARM:OPEN
   s" a retained compiler copies cached literals into the new AOT window" T-LABEL
   fresh AOT-ARM:D0 @ >= TTRUE
   fresh old T<>
   old N>BYTES 9 s" lone-body" STR= TTRUE
   fresh again T=
   s" lone-body" NSTR:INTERN {: newest:n :}
   newest fresh T= ;

\ ---- what a literal costs in code bytes ---------------------------------------
\ The payload lives in DATA space, so the two chain emissions have the same code
\ length even though one string is much longer. Address materialization depends
\ on the mapped DATA base, so the relation is the stable claim.
: COST-PAIR ( -- )
   S\" : NST-COST-SHORT ( -- ptr u8 n ) s\" hi\" ;" DEF
   S\" : NST-COST-LONG ( -- ptr u8 n ) s\" 12345678901234567890123456789012\" ;" DEF ;

: CODE-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-FIND XREF-LEN ;

: BYTE-COST-CASE ( -- )
   COST-PAIR
   s" the chain's code does not grow with the string at all" T-LABEL
   s" NST-COST-LONG" CODE-LEN  s" NST-COST-SHORT" CODE-LEN T= ;

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
   WINDOW-CASE
   ROLLBACK-CASE
   BYTE-COST-CASE
   CAP-CASE
   T-REPORT ;

;package

NSTRING-TEST:RUN
