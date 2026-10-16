\ request.f - real Wasm fixture for the browser request/reply protocol.
\ Build with tools/wasm-build.f, entry REQUEST-TURN:TURN.

require src/lib/errors.f
require src/lib/string.f

package REQUEST-TURN
private

CAST: >BYTES ( n -- ptr u8 )
CAST: BYTES>N ( ptr u8 -- n )
CAST: >CELLS ( ptr u8 -- ptr n )

56 BUFFER: EVENT-BUF
128 BUFFER: REQ
128 BUFFER: REQ2
variable REQ-U
512 BUFFER: TEXT
variable TEXT-U

: FIELD, ( n -- )
   8 0 do dup $FF and emit 8 rshift loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD, a FIELD, b FIELD, c FIELD, ;

: EVENT@ ( n -- n )
   8 * EVENT-BUF + >CELLS @ ;

: SAY-C ( n -- )
   TEXT TEXT-U @ + c! TEXT-U @ 1+ TEXT-U ! ;

: DIGITS ( n -- )
   {: n:n :}
   n 10 / {: q:n :}
   q 0 <> if q RECURSE then
   n q 10 * - 48 + SAY-C ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: REQ-C ( n -- )
   REQ REQ-U @ + c! REQ-U @ 1+ REQ-U ! ;

: REQ-CELL ( n -- )
   8 0 do dup $FF and REQ-C 8 rshift loop drop ;

: REQ+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ REQ-C loop ;

: BOM ( -- )
   $EF REQ-C $BB REQ-C $BF REQ-C ;

: REQUEST ( n -- )
   {: id:n :}
   7 REQ BYTES>N REQ-U @ id RECORD ;

: IS? ( n n ptr u8 n -- bool )
   {: at:n u:n t:ptr tu:n :}
   u tu <> if false exit then
   tu 0 ?do at i + >BYTES c@ t i + c@ <> if false unloop exit then loop
   true ;

: MAKE ( -- )
   0 REQ-U !
   4 REQ-CELL 6 REQ-CELL
   s" POST" REQ+ s" /state" REQ+
   0 REQ-C $FF REQ-C ;

: TYPED ( -- )
   1 EVENT@ 2 EVENT@ {: at:n u:n :}
   MAKE
   at u s" trap" IS? if -1 >BYTES c@ drop exit then
   at u s" short header" IS? if 7 REQ BYTES>N 15 $20000000000001 RECORD exit then
   at u s" outside" IS? if 7 -1 REQ-U @ $20000000000001 RECORD exit then
   at u s" huge length" IS? if
      8 0 do $FF REQ i + c! loop
   then
   at u s" bad length" IS? if  $FF REQ c! then
   at u s" bad id" IS? if  0 REQUEST exit then
   at u s" negative id" IS? if  -1 REQUEST exit then
   at u s" empty method" IS? if  0 REQ c! then
   at u s" bad token" IS? if  32 REQ 16 + c! then
   at u s" bad newline" IS? if  10 REQ 19 + c! then
   at u s" bad utf8" IS? if  $FF REQ 20 + c! then
   at u s" empty path" IS? if  0 REQ 8 + c! then
   at u s" get body" IS? if
      0 REQ-U ! 3 REQ-CELL 6 REQ-CELL
      s" GET" REQ+ s" /state" REQ+ 1 REQ-C
      $20000000000001 REQUEST exit
   then
   at u s" bom method" IS? if
      0 REQ-U ! 7 REQ-CELL 6 REQ-CELL
      BOM s" POST" REQ+ s" /state" REQ+
      $20000000000001 REQUEST exit
   then
   at u s" bom only method" IS? if
      0 REQ-U ! 3 REQ-CELL 6 REQ-CELL
      BOM s" /state" REQ+
      $20000000000001 REQUEST exit
   then
   at u s" bom path" IS? if
      0 REQ-U ! 4 REQ-CELL 9 REQ-CELL
      s" POST" REQ+ BOM s" /state" REQ+
      $20000000000001 REQUEST exit
   then
   at u s" legacy post" IS? if
      0 REQ-U ! s" /state" REQ+ 0 REQ-C $FF REQ-C
      4 REQ BYTES>N REQ-U @ 6 RECORD exit
   then
   at u s" legacy fetch" IS? if
      s" /state" {: path:ptr len:n :}
      1 path BYTES>N len 0 RECORD exit
   then
   at u s" legacy two" IS? if
      0 REQ-U ! s" /state" REQ+ 0 REQ-C $FF REQ-C
      4 REQ BYTES>N REQ-U @ 6 RECORD
      s" /state" {: path:ptr len:n :}
      1 path BYTES>N len 0 RECORD exit
   then
   at u s" two" IS? if
      $20000000000001 REQUEST $20000000000002 REQUEST exit
   then
   at u s" parallel" IS? if
      REQ-U @ 0 ?do REQ i + c@ REQ2 i + c! loop
      7 REQ2 BYTES>N REQ-U @ $20000000000001 RECORD
      0 REQ-U ! 3 REQ-CELL 9 REQ-CELL
      s" GET" REQ+ s" /projects" REQ+
      $20000000000002 REQUEST exit
   then
   at u s" interleaved" IS? if
      REQ-U @ 0 ?do REQ i + c@ REQ2 i + c! loop
      7 REQ2 BYTES>N REQ-U @ $20000000000001 RECORD
      s" /legacy*" {: span:ptr len:n :}
      4 span BYTES>N len 7 RECORD
      0 REQ-U ! 3 REQ-CELL 9 REQ-CELL
      s" GET" REQ+ s" /projects" REQ+
      $20000000000002 REQUEST exit
   then
   at u s" get" IS? if
      0 REQ-U ! 3 REQ-CELL 9 REQ-CELL
      s" GET" REQ+ s" /projects" REQ+
   then
   at u s" put" IS? if
      0 REQ-U ! 3 REQ-CELL 5 REQ-CELL
      s" PUT" REQ+ s" /view" REQ+ 0 REQ-C $FF REQ-C
   then
   at u s" get" IS? if $20000000000002
   else at u s" put" IS? if $20000000000003
   else $20000000000001 then then
   REQUEST ;

: ANSWERED ( -- )
   3 EVENT@ 470 = if 99 0 0 0 RECORD exit then
   3 EVENT@ 471 = if -1 >BYTES c@ drop exit then
   0 TEXT-U !
   4 EVENT@ DIGITS 32 SAY-C 3 EVENT@ DIGITS 32 SAY-C
   5 EVENT@ DIGITS 44 SAY-C 6 EVENT@ DIGITS 32 SAY-C
   1 EVENT@ 2 EVENT@ {: at:n u:n :}
   u 0 ?do at i + >BYTES c@ DIGITS 44 SAY-C loop
   TEXT-RECORD
   3 EVENT@ 202 = if
      0 REQ-U ! s" /followup" REQ+ 42 REQ-C
      4 REQ BYTES>N REQ-U @ 9 RECORD
   then ;

: BYTES ( -- )
   0 TEXT-U !
   5 EVENT@ DIGITS 44 SAY-C 6 EVENT@ DIGITS
   TEXT-RECORD ;

public

: TURN ( -- )
   0 EVENT@ {: kind:n :}
   kind 0 = if 0 EVENT-BUF BYTES>N 0 0 RECORD exit then
   kind 4 = if TYPED exit then
   kind 3 = if ANSWERED exit then
   kind 1 = if BYTES exit then ;

;package
