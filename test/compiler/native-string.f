\ native-string.f - the string literal store: its pools, segments and rows.

require src/lib/test.f
require src/lib/string.f
require src/core/generated-declaration.f
require src/habu/intern.f
require test/compiler/literal-segment.f

package NSTRING-TEST

\ A failed evaluate-closed rewinds DATA but keeps the compiler's literal table.
\ Reclaim that DATA and verify the cached bytes still belong to its pool.
public
variable SAVED-LITERAL
private
create ROLLBACK-BYTES 101 c, 112 c, 104 c, 101 c, 109 c, 101 c, 114 c, 97 c, 108 c,
: PTR>N ( ptr a -- n ) NULL-PTR - ;
\ A saved literal address is an integer cell; reading its bytes needs a byte view.
CAST: N>BYTES ( n -- ptr u8 )

: FAILED-INTERN ( -- )
   S\" s\q ephemeral\q INTERN:STRING NSTRING-TEST:SAVED-LITERAL ! 77 throw" evaluate-closed ;

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
   s" NST-ROLLBACK drop NULL-PTR -" TEST-EVAL:N SAVED-LITERAL @ T= ;

variable MUTABLE-CELL

: FOREIGN-POOL ( -- ) MUTABLE-CELL BYTE-VIEW INTERN:SWITCH ;

: SWITCH-CASE ( -- )
   s" switching pools preserves addresses and rebuilds the intern index" T-LABEL
   INTERN:WINDOW-OPEN
   s" first-pool-body" INTERN:STRING {: first:n :}
   INTERN:ACTIVE {: original:ptr :}
   INTERN:WINDOW-OPEN
   s" second-pool-body" INTERN:STRING {: second:n :}
   INTERN:ACTIVE {: other:ptr :}
   original INTERN:SWITCH
   INTERN:COUNT 1 T=
   s" first-pool-body" INTERN:STRING first T=
   INTERN:COUNT 1 T=
   second INTERN:REINTERN-OWNED TTRUE {: imported:n :}
   imported second T<>
   s" second-pool-body" INTERN:STRING imported T=
   INTERN:COUNT 2 T=
   other INTERN:SWITCH
   s" second-pool-body" INTERN:STRING second T=
   INTERN:COUNT 1 T=
   original INTERN:SWITCH
   s" first-pool-body" INTERN:STRING first T=
   s" a foreign owner is refused without changing the active pool" T-LABEL
   [: FOREIGN-POOL ;] E-NSTR-BODY TTHROWSQ
   INTERN:ACTIVE original = TTRUE
   s" second-pool-body" INTERN:STRING imported T=
   INTERN:COUNT 2 T= ;

: OWNED-ROW ( n ptr u8 n -- ) {: address:n body:ptr size:n :}
   address INTERN:OWNER-ROW TTRUE
   size T= PTR>N body PTR>N T= ;

: UNOWNED-ROW ( n -- )
   INTERN:OWNER-ROW TFALSE
   0 T= PTR>N 0 T= ;

: OWNER-CASE ( -- )
   s" row ownership survives a window opened at an odd DATA cursor" T-LABEL
   align 0 c,
   INTERN:WINDOW-OPEN
   INTERN:COUNT 0 T=  INTERN:BYTES 0 T=
   s" exact-row" INTERN:STRING {: old:n :}
   old old N>BYTES 9 OWNED-ROW
   old 1+ old N>BYTES 9 OWNED-ROW
   old 8 + old N>BYTES 9 OWNED-ROW
   old 1- UNOWNED-ROW
   old 9 + UNOWNED-ROW

   s" empty rows have a distinct valid address and cost zero body bytes" T-LABEL
   s" " INTERN:STRING {: empty:n :}
   empty empty N>BYTES 0 OWNED-ROW
   empty old T<>
   empty 1- UNOWNED-ROW
   empty 1+ UNOWNED-ROW
   INTERN:COUNT 2 T=  INTERN:BYTES 9 T=

   INTERN:WINDOW-OPEN
   old 2 + old N>BYTES 9 OWNED-ROW
   empty empty N>BYTES 0 OWNED-ROW
   s" exact-row" INTERN:STRING {: fresh:n :}
   fresh old T<>
   old 2 + INTERN:REINTERN-OWNED TTRUE fresh 2 + T=
   old 8 + INTERN:REINTERN-OWNED TTRUE fresh 8 + T=
   INTERN:COUNT 1 T=  INTERN:BYTES 9 T=
   empty INTERN:REINTERN-OWNED TTRUE {: new-empty:n :}
   new-empty empty T<>
   new-empty new-empty N>BYTES 0 OWNED-ROW
   INTERN:COUNT 2 T=  INTERN:BYTES 9 T=

   s" ordinary DATA and arbitrary numbers never acquire literal ownership" T-LABEL
   MUTABLE-CELL PTR>N UNOWNED-ROW
   0 UNOWNED-ROW  -1 UNOWNED-ROW
   MUTABLE-CELL PTR>N INTERN:REINTERN-OWNED TFALSE MUTABLE-CELL PTR>N T= ;

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
   [: LITERAL-SEGMENT:OPEN 77 throw ;] DECLARATION-TRANSACTION:RUN ;

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
   k FILL-BODY INTERN:STRING  k cells FILL-AT + !
   1 FILL-N +! ;

: FILL ( -- )
   20000 0 ?do i FILL-ONE loop ;

: FILL-CHECK ( n -- ) {: k:n :}
   k cells FILL-AT + @ N>BYTES 64  k FILL-BODY  STR= if 1 FILLED +! then ;

: BIG! ( n -- ) {: u:n :}
   here BIG-AT !  u allot  u BIG-U !
   u 0 ?do 66 BIG-AT @ i + c! loop ;

: BIG-INTERN ( -- )
   BIG-AT @ BIG-U @ INTERN:STRING drop ;

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
   INTERN:COUNT {: rows:n :}
   here PTR>N {: at:n :}
   [: BIG-INTERN ;] E-NSTR-CAP TTHROWSQ
   INTERN:COUNT rows T=
   here PTR>N at T= ;

\ ---- a closed window's pool ----------------------------------------------------
\ A stripped link copies into the window's pool each body it reaches in another
\ pool, after the window's end is latched (src/habu/aot-window-latch.f
\ AOT-DATA-SPAN; test/stripped-literal.f links through it). Closing a full pool
\ opens one more segment for those copies, inside the window, and none after it:
\ a segment opened past the window's end would leave its copies out of the image,
\ so a body past the room the pool kept is refused by name.
: OVERFLOW-INTERN ( -- )
   FILL-BUF 1 INTERN:STRING drop ;

: CLOSE-CASE ( -- )
   s" closing a full pool opens it one more segment" T-LABEL
   INTERN:ACTIVE {: original:ptr :}
   INTERN:WINDOW-OPEN
   $80000 BIG!
   BIG-INTERN
   INTERN:WINDOW-CLOSE
   INTERN:SEGMENTS 2 T=
   s" which takes a whole arena" T-LABEL
   [: BIG-INTERN ;] 0 TTHROWSQ
   s" and a body past it is refused by name, opening nothing" T-LABEL
   here PTR>N {: at:n :}
   [: OVERFLOW-INTERN ;] E-NSTR-CAP TTHROWSQ
   INTERN:SEGMENTS 2 T=
   here PTR>N at T=
   original INTERN:SWITCH ;

public

: RUN ( -- )
   T-RESET
   SWITCH-CASE
   ROLLBACK-CASE
   OWNER-CASE
   EVAL-SEGMENT-CASE
   DECL-SEGMENT-CASE
   CAP-CASE
   CLOSE-CASE
   T-REPORT ;

;package

NSTRING-TEST:RUN
