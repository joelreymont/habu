\ native-string.f - a string literal, compiled by the native chain and run.

\ Tier 1 first: one address per interned body, and code that does not grow with
\ the string, are facts of the optimizing compiler's literal emission.
1 set-tier

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

variable MUTABLE-CELL

: FOREIGN-POOL ( -- ) MUTABLE-CELL BYTE-VIEW NSTR:SWITCH ;

: SWITCH-CASE ( -- )
   s" switching pools preserves addresses and rebuilds the intern index" T-LABEL
   NSTR:WINDOW-OPEN
   s" first-pool-body" NSTR:INTERN {: first:n :}
   NSTR:ACTIVE {: original:ptr :}
   NSTR:WINDOW-OPEN
   s" second-pool-body" NSTR:INTERN {: second:n :}
   NSTR:ACTIVE {: other:ptr :}
   original NSTR:SWITCH
   NSTR:COUNT 1 T=
   s" first-pool-body" NSTR:INTERN first T=
   NSTR:COUNT 1 T=
   second NSTR:REINTERN-OWNED TTRUE {: imported:n :}
   imported second T<>
   s" second-pool-body" NSTR:INTERN imported T=
   NSTR:COUNT 2 T=
   other NSTR:SWITCH
   s" second-pool-body" NSTR:INTERN second T=
   NSTR:COUNT 1 T=
   original NSTR:SWITCH
   s" first-pool-body" NSTR:INTERN first T=
   s" a foreign owner is refused without changing the active pool" T-LABEL
   [: FOREIGN-POOL ;] E-NSTR-BODY TTHROWSQ
   NSTR:ACTIVE original = TTRUE
   s" second-pool-body" NSTR:INTERN imported T=
   NSTR:COUNT 2 T= ;

: OWNED-ROW ( n ptr u8 n -- ) {: address:n body:ptr size:n :}
   address NSTR:OWNER-ROW TTRUE
   size T= PTR>N body PTR>N T= ;

: UNOWNED-ROW ( n -- )
   NSTR:OWNER-ROW TFALSE
   0 T= PTR>N 0 T= ;

: OWNER-CASE ( -- )
   s" row ownership survives a window opened at an odd DATA cursor" T-LABEL
   align 0 c,
   NSTR:WINDOW-OPEN
   NSTR:COUNT 0 T=  NSTR:BYTES 0 T=
   s" exact-row" NSTR:INTERN {: old:n :}
   old old N>BYTES 9 OWNED-ROW
   old 1+ old N>BYTES 9 OWNED-ROW
   old 8 + old N>BYTES 9 OWNED-ROW
   old 1- UNOWNED-ROW
   old 9 + UNOWNED-ROW

   s" empty rows have a distinct valid address and cost zero body bytes" T-LABEL
   s" " NSTR:INTERN {: empty:n :}
   empty empty N>BYTES 0 OWNED-ROW
   empty old T<>
   empty 1- UNOWNED-ROW
   empty 1+ UNOWNED-ROW
   NSTR:COUNT 2 T=  NSTR:BYTES 9 T=

   NSTR:WINDOW-OPEN
   old 2 + old N>BYTES 9 OWNED-ROW
   empty empty N>BYTES 0 OWNED-ROW
   s" exact-row" NSTR:INTERN {: fresh:n :}
   fresh old T<>
   old 2 + NSTR:REINTERN-OWNED TTRUE fresh 2 + T=
   old 8 + NSTR:REINTERN-OWNED TTRUE fresh 8 + T=
   NSTR:COUNT 1 T=  NSTR:BYTES 9 T=
   empty NSTR:REINTERN-OWNED TTRUE {: new-empty:n :}
   new-empty empty T<>
   new-empty new-empty N>BYTES 0 OWNED-ROW
   NSTR:COUNT 2 T=  NSTR:BYTES 9 T=

   s" ordinary DATA and arbitrary numbers never acquire literal ownership" T-LABEL
   MUTABLE-CELL PTR>N UNOWNED-ROW
   0 UNOWNED-ROW  -1 UNOWNED-ROW
   MUTABLE-CELL PTR>N NSTR:REINTERN-OWNED TFALSE MUTABLE-CELL PTR>N T= ;

\ ROUTINE$ was compiled into the engine by its retained build host. Its literal
\ predates this load and belongs to the transferred source pool after seeding.
: SEEDED-OWNER-CASE ( -- )
   s" the rebuilt target owns literals compiled by its retained host" T-LABEL
   NTRAP:ROUTINE$ {: body:ptr size:n :}
   body size s" die" T$=
   body PTR>N body size OWNED-ROW
   NSTR:WINDOW-OPEN
   body PTR>N body size OWNED-ROW
   body PTR>N 1+ NSTR:REINTERN-OWNED TTRUE
   s" die" NSTR:INTERN 1+ T= ;

variable BAD-ROWS
variable BAD-OFFSET
variable BAD-LENGTH
PTR-VARIABLE IMPORTED-POOL
: SELECT-IMPORTED ( -- ) IMPORTED-POOL @ NSTR:SWITCH ;

\ This negative fixture reaches the real private importer by its dictionary
\ identity. It never grants ownership: every supplied row is malformed.
TRUSTED: IMPORT-XT ( n -- [ ptr u8 n ptr n ptr n -- ] ) ;

: IMPORT-OP ( -- [ ptr u8 n ptr n ptr n -- ] )
   s" NSTR" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? TTRUE
   XREF-LEN {: wid:n :}
   s" IMPORT-ROWS" wid XREF-FIND-WL
   dup XREF-FOUND? TTRUE
   XREF-START IMPORT-XT ;

: BAD-IMPORT ( -- )
   MUTABLE-CELL BYTE-VIEW BAD-ROWS @ BAD-OFFSET BAD-LENGTH IMPORT-OP execute ;

: REJECT-IMPORT ( -- )
   here PTR>N {: before:n :}
   [: BAD-IMPORT ;] E-NSTR-BODY TTHROWSQ
   here PTR>N before T=
   MUTABLE-CELL PTR>N UNOWNED-ROW
   NTRAP:ROUTINE$ {: body:ptr size:n :}
   body PTR>N body size OWNED-ROW ;

: IMPORT-CASE ( -- )
   s" only the build driver can invoke literal ownership import" T-LABEL
   s" NSTR:IMPORT-ROWS" XREF-FIND XREF-FOUND? TFALSE
   \ The checker reports an unknown public name (1), never a certificate (-1).
   s" NST-PUBLIC-IMPORT ( ptr u8 n ptr n ptr n -- ) NSTR:IMPORT-ROWS"
   CHECK-CANDIDATE! 1 T=

   s" malformed imports leave DATA and existing row ownership unchanged" T-LABEL
   -1 BAD-ROWS ! REJECT-IMPORT
   1 BAD-ROWS ! -1 BAD-OFFSET ! 0 BAD-LENGTH ! REJECT-IMPORT
   0 BAD-OFFSET ! $7FFFFFFFFFFFFFFF BAD-LENGTH ! REJECT-IMPORT
   $80000 BAD-OFFSET ! 1 BAD-LENGTH ! REJECT-IMPORT

   s" imported compact owners cannot become writable pools" T-LABEL
   NSTR:ACTIVE {: active:ptr :}
   0 BAD-OFFSET ! 0 BAD-LENGTH !
   align here BYTE-VIEW IMPORTED-POOL !
   MUTABLE-CELL BYTE-VIEW 1 BAD-OFFSET BAD-LENGTH IMPORT-OP execute
   [: SELECT-IMPORTED ;] E-NSTR-BODY TTHROWSQ
   NSTR:ACTIVE active = TTRUE ;

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
   SWITCH-CASE
   ROLLBACK-CASE
   OWNER-CASE
   SEEDED-OWNER-CASE
   IMPORT-CASE
   BYTE-COST-CASE
   CAP-CASE
   T-REPORT ;

;package

NSTRING-TEST:RUN
