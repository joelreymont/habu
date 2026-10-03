\ native-string.f - a string literal, compiled by the native chain and run.

require lib/test.f
require lib/string.f
require lib/fmt.f
require src/core/generated-declaration.f
require src/compiler/native/compiler.f
require src/compiler/native/string.f
require src/habu/aot-arm.f
require test/compiler/literal-segment.f

\ Tier 1 below: one address per interned body, and code that does not grow with
\ the string, are facts of the optimizing compiler's literal emission.
1 set-tier

package NSTRING-TEST

private

\ ---- the words the chain compiles --------------------------------------------
\ Each is compiled through the production entry, which reads what the definition
\ takes and leaves off the checker's certificate: `( -- ptr u8 n )` is TWO, an
\ address and a length, and nothing here states it.
\ Callers of these words, which did not exist when the suite was compiled, run
\ through `TEST-EVAL:N`, or `TEST-EVAL:FLAG` for the flag most of these
\ questions answer with.

: PLAIN ( -- )
   S\" : NST-PLAIN ( -- ptr u8 n ) s\" hi\" ;" evaluate-closed ;

: EMPTY ( -- )
   S\" : NST-EMPTY ( -- ptr u8 n ) s\" \" ;" evaluate-closed ;

\ A body built to fool a reader that re-lexed it: two spaces that must not
\ collapse, a definition closer, a colon and a comment opener that must not
\ become syntax, and a trailing word that must not become a token.
: HOSTILE ( -- )
   S\" : NST-HOSTILE ( -- ptr u8 n ) s\" a  b ; : ( x\" ;" evaluate-closed ;

\ A named escape, a hex escape and a quote escape in one body. The quote escape
\ is the one a decoder that merely scanned for the closing quote would get wrong.
: ESCAPED ( -- )
   S\" : NST-ESCAPED ( -- ptr u8 n ) s\\\" a\\tb\\x41\\qc\" ;" evaluate-closed ;

\ Two sites in ONE definition, writing the same body, so the addresses they push
\ can be compared against each other.
: TWICE ( -- )
   S\" : NST-TWICE ( -- ptr u8 n ptr u8 n ) s\" dup\" s\" dup\" ;" evaluate-closed ;

\ And a second definition writing that same body, so the sharing can be shown to
\ cross a definition boundary and not only a site boundary.
: SHARED ( -- )
   S\" : NST-SHARED ( -- ptr u8 n ) s\" dup\" ;" evaluate-closed ;

\ A body nothing else in this suite writes, for the counting cases, and a second
\ definition writing exactly it.
: LONE ( -- )
   S\" : NST-LONE ( -- ptr u8 n ) s\" lone-body\" ;" evaluate-closed ;

: LONE-AGAIN ( -- )
   S\" : NST-LONE2 ( -- ptr u8 n ) s\" lone-body\" ;" evaluate-closed ;

\ ---- what the published words answer -----------------------------------------
: ROUND-TRIP-CASE ( -- )
   PLAIN
   s" a string literal compiles through the chain and pushes its bytes" T-LABEL
   S\" NST-PLAIN s\q hi\q STR=" TEST-EVAL:FLAG TTRUE
   s" NST-PLAIN nip" TEST-EVAL:N 2 T=

   EMPTY
   s" the empty string literal pushes a valid address and zero" T-LABEL
   s" NST-EMPTY nip" TEST-EVAL:N 0 T=
   s" NST-EMPTY drop 0 >" TEST-EVAL:FLAG TTRUE

   HOSTILE
   s" a body that would re-lex as syntax survives verbatim" T-LABEL
   S\" NST-HOSTILE s\q a  b ; : ( x\q STR=" TEST-EVAL:FLAG TTRUE
   s" NST-HOSTILE nip" TEST-EVAL:N 12 T=

   ESCAPED
   s" an escaped body arrives decoded, not as the text it was written as" T-LABEL
   S\" NST-ESCAPED S\\\q a\\tbA\\qc\q STR=" TEST-EVAL:FLAG TTRUE
   s" NST-ESCAPED nip" TEST-EVAL:N 6 T= ;

\ ---- one body, one address ----------------------------------------------------
\ THE SHARING IS THE POINT AND NOT AN ECONOMY. A store that answered a fresh
\ address for equal bytes would leak a copy every time the pipeline re-elaborated
\ a definition it had refused, so this is the property that stands in for "a
\ refusal moves nothing" over the arena.
: SHARING-CASE ( -- )
   TWICE
   s" two sites writing one body push one address" T-LABEL
   s" NST-TWICE drop swap drop =" TEST-EVAL:FLAG TTRUE

   SHARED
   s" a second definition writing that body pushes the same address" T-LABEL
   s" NST-SHARED drop NST-TWICE drop nip drop =" TEST-EVAL:FLAG TTRUE

   s" and a different body pushes a different address" T-LABEL
   s" NST-SHARED drop NST-PLAIN drop <>" TEST-EVAL:FLAG TTRUE ;

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
   S\" NST-LONE2 s\q lone-body\q STR=" TEST-EVAL:FLAG TTRUE
   s" NST-LONE drop NST-LONE2 drop =" TEST-EVAL:FLAG TTRUE ;

\ A failed evaluate-closed rewinds DATA but keeps the compiler's literal table.
\ Reclaim that DATA and verify the cached bytes still belong to its pool.
public
variable SAVED-LITERAL
private
create ROLLBACK-BYTES 101 c, 112 c, 104 c, 101 c, 109 c, 101 c, 114 c, 97 c, 108 c,
: PTR>N ( ptr a -- n ) BYTE-VIEW NULL-PTR BYTE-VIEW - ;
\ A saved literal address is an integer cell; reading its bytes needs a byte view.
CAST: N>BYTES ( n -- ptr u8 )

: FAILED-INTERN ( -- )
   S\" s\q ephemeral\q NSTR:INTERN NSTRING-TEST:SAVED-LITERAL ! 77 throw" evaluate-closed ;

: ROLLBACK-CASE ( -- )
   s" literals survive DATA rollback after failed evaluation" T-LABEL
   here PTR>N {: before:n :}
   [: FAILED-INTERN ;] 77 TTHROWSQ
   SAVED-LITERAL @ before < TTRUE
   here {: reclaimed:ptr :}
   128 allot
   128 0 ?do 88 reclaimed i + c! loop
   S\" : NST-ROLLBACK ( -- ptr u8 n ) s\q ephemeral\q ;" evaluate-closed
   SAVED-LITERAL @ N>BYTES 9 ROLLBACK-BYTES 9 STR= TTRUE
   s" NST-ROLLBACK drop" TEST-EVAL:N SAVED-LITERAL @ T= ;

: WINDOW-CASE ( -- )
   s" lone-body" NSTR:INTERN {: old:n :}
   data-base AOT-CELLS:B0-CELL + @ {: b0:n :}
   data-base AOT-CELLS:D0-CELL + @ {: d0:n :}
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
\ The dictionary row holds the importer's code address as an integer.
CAST: IMPORT-XT ( n -- [ ptr u8 n ptr n ptr n -- ] )

: IMPORT-OP ( -- [ ptr u8 n ptr n ptr n -- ] )
   s" NSTR" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? TTRUE
   XREF-PKG-PRIVATE {: wid:n :}
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
   S\" : NST-COST-SHORT ( -- ptr u8 n ) s\" hi\" ;" evaluate-closed
   S\" : NST-COST-LONG ( -- ptr u8 n ) s\" 12345678901234567890123456789012\" ;" evaluate-closed ;

: CODE-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-FIND XREF-LEN ;

: BYTE-COST-CASE ( -- )
   COST-PAIR
   s" the chain's code does not grow with the string at all" T-LABEL
   s" NST-COST-LONG" CODE-LEN  s" NST-COST-SHORT" CODE-LEN T= ;

\ ---- many long literals ---------------------------------------------------------
\ Eighty definitions each push their own 7000-byte body: 560,000 bytes, more than
\ one segment's arena holds, so the store opens another while the chain compiles.
$2000 constant SRC-CAP               \ one definition around a 7000-byte body
SRC-CAP BUFFER: SRC
variable SRC-U
variable LONG-N                      \ definitions compiled
variable LONG-OK                     \ definitions that pushed their own body

: SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SRC-U @ u + SRC-CAP > if E-STR-CAPACITY throw then
   a  SRC SRC-U @ +  u BYTE-COPY
   u SRC-U +! ;

: NUM+ ( n -- )
   SB-RESET FMT:SB-U SB$ SRC+ ;

\ Body k: the name of the definition that pushes it, then `z` up to 7000 bytes.
: LONG-BODY+ ( n -- ) {: k:n :}
   SRC-U @ {: at:n :}
   s" NSL" SRC+  k NUM+
   7000  SRC-U @ at -  -  0 ?do s" z" SRC+ loop ;

\ The body LONG-ANSWERS expects, public only because its closed text reaches it
\ qualified.
public
: LONG-BODY$ ( -- ptr u8 n )
   SRC SRC-U @ ;
private

: LONG-DEF ( n -- ) {: k:n :}
   0 SRC-U !
   s" : NSL" SRC+  k NUM+  S\"  ( -- ptr u8 n ) s\q " SRC+
   k LONG-BODY+  S\" \q ;" SRC+
   SRC SRC-U @ evaluate-closed
   1 LONG-N +! ;

: LONG-DEFS ( -- )
   80 0 ?do i LONG-DEF loop ;

: LONG-ANSWERS ( n -- ) {: k:n :}
   0 SRC-U !  k LONG-BODY+
   SB-RESET s" NSL" SB-APPEND k FMT:SB-U s"  NSTRING-TEST:LONG-BODY$ STR=" SB-APPEND
   SB$ TEST-EVAL:FLAG if 1 LONG-OK +! then ;

: LONG-LITERALS-CASE ( -- )
   s" eighty definitions of distinct 7000-byte bodies compile" T-LABEL
   0 LONG-N !
   [: LONG-DEFS ;] 0 TTHROWSQ
   LONG-N @ 80 T=
   s" and each pushes its own body, as tier 0 does" T-LABEL
   0 LONG-OK !
   LONG-N @ 0 ?do i LONG-ANSWERS loop
   LONG-OK @ 80 T= ;

\ ---- a segment opened inside work that is undone -------------------------------
\ The store opens a segment at `here`, inside whatever is running. A rewind of
\ that work - a failed evaluation, a rolled-back declaration, a failed REPL line
\ (test/repl-literal-segment.f) - stops at the engine's DATA floor, so the
\ segment and every address it answered outlive it.
: EVAL-SEGMENT-CASE ( -- )
   s" a segment opened inside a failed evaluation outlives its rewind" T-LABEL
   [: s" LITERAL-SEGMENT:OPEN 77 throw" evaluate-closed ;] 77 TTHROWSQ
   LITERAL-SEGMENT:SURVIVED? TTRUE ;

: DECL-BODY ( -- )
   [: LITERAL-SEGMENT:OPEN 77 throw ;] GENERATED-DECL:RUN ;

: DECL-SEGMENT-CASE ( -- )
   s" a segment opened in a rolled-back declaration outlives the rollback" T-LABEL
   [: DECL-BODY ;] 77 TTHROWSQ
   LITERAL-SEGMENT:SURVIVED? TTRUE ;

\ ---- the store's ceiling ------------------------------------------------------
\ A pool grows by segments, so the count and the total size of its bodies are
\ bounded by DATA alone. 20000 bodies of 64 bytes pass both of a segment's
\ ceilings, its rows and its arena bytes, twice over. One body larger than a
\ segment's arena is refused by name, before anything moves: the addresses
\ already answered are compiled into published routines.
create FILL-BUF 64 allot
create FILL-AT 20000 cells allot
variable FILL-N                      \ bodies interned, so a refusal reads none back
variable FILLED
variable BIG-U
PTR-VARIABLE BIG-AT

: FILL-BODY ( n -- ptr u8 n ) {: k:n :}
   8 0 ?do
      k i 8 * rshift $FF and  FILL-BUF i +  c!
   loop
   FILL-BUF 64 ;

: FILL-ONE ( n -- ) {: k:n :}
   k FILL-BODY NSTR:INTERN  k cells FILL-AT + !
   1 FILL-N +! ;

: FILL ( -- )
   20000 0 ?do i FILL-ONE loop ;

: FILL-CHECK ( n -- ) {: k:n :}
   k cells FILL-AT + @ N>BYTES 64  k FILL-BODY  STR= if 1 FILLED +! then ;

: BIG! ( n -- ) {: u:n :}
   here BIG-AT !  u allot  u BIG-U !
   u 0 ?do 66 BIG-AT @ i + c! loop ;

: BIG-INTERN ( -- )
   BIG-AT @ BIG-U @ NSTR:INTERN drop ;

: CAP-CASE ( -- )
   s" twenty thousand bodies past both of a segment's ceilings intern" T-LABEL
   0 FILL-N !
   [: FILL ;] 0 TTHROWSQ
   s" and each reads back as its own bytes" T-LABEL
   0 FILLED !
   FILL-N @ 0 ?do i FILL-CHECK loop
   FILLED @ 20000 T=
   s" a body as large as a segment's arena interns" T-LABEL
   $80000 BIG!
   [: BIG-INTERN ;] 0 TTHROWSQ
   s" one byte larger is refused by name and leaves no trace" T-LABEL
   $80001 BIG!
   NSTR:COUNT {: rows:n :}
   here PTR>N {: at:n :}
   [: BIG-INTERN ;] E-NSTR-CAP TTHROWSQ
   NSTR:COUNT rows T=
   here PTR>N at T= ;

\ ---- a closed window's pool ----------------------------------------------------
\ A stripped link copies into the window's pool each body it reaches in another
\ pool, after the window's end is latched (src/habu/aot-window-latch.f
\ AOT-DATA-SPAN; test/stripped-literal.f links through it). Closing a full pool
\ opens one more segment for those copies, inside the window, and none after it:
\ a segment opened past the window's end would leave its copies out of the image,
\ so a body past the room the pool kept is refused by name.
: OVERFLOW-INTERN ( -- )
   FILL-BUF 1 NSTR:INTERN drop ;

: CLOSE-CASE ( -- )
   s" closing a full pool opens it one more segment" T-LABEL
   NSTR:ACTIVE {: original:ptr :}
   NSTR:WINDOW-OPEN
   $80000 BIG!
   BIG-INTERN
   NSTR:WINDOW-CLOSE
   NSTR:SEGMENTS 2 T=
   s" which takes a whole arena" T-LABEL
   [: BIG-INTERN ;] 0 TTHROWSQ
   s" and a body past it is refused by name, opening nothing" T-LABEL
   here PTR>N {: at:n :}
   [: OVERFLOW-INTERN ;] E-NSTR-CAP TTHROWSQ
   NSTR:SEGMENTS 2 T=
   here PTR>N at T=
   original NSTR:SWITCH ;

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
   LONG-LITERALS-CASE
   EVAL-SEGMENT-CASE
   DECL-SEGMENT-CASE
   CAP-CASE
   CLOSE-CASE
   T-REPORT ;

;package

NSTRING-TEST:RUN
