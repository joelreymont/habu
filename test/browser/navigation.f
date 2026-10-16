\ navigation.f - Wasm fixture for the browser navigation, tree and highlight
\ protocol. Build with tools/wasm-build.f, entry NAV:TURN.
require src/lib/string.f

package NAV
private

CAST: >BYTES ( n -- ptr u8 )
CAST: >CELLS ( ptr u8 -- ptr n )
CAST: BYTES>N ( ptr u8 -- n )

56 BUFFER: EVENT-BUF
256 BUFFER: TEXT
variable TEXT-U
variable PRESS-BUTTONS
variable PRESS-MODS
72 BUFFER: TRIANGLES
64 BUFFER: MATRIX
3 BUFFER: MASK

: FIELD, ( n -- )
   8 0 do dup $FF and emit 8 rshift loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD, a FIELD, b FIELD, c FIELD, ;

: EVENT@ ( n -- n )
   8 * EVENT-BUF + >CELLS @ ;

: CHAR+ ( n -- )
   TEXT TEXT-U @ + c! TEXT-U @ 1+ TEXT-U ! ;

: SAY ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ CHAR+ loop ;

: DIGITS ( n -- )
   {: v:n :}
   v 10 / {: q:n :}
   q 0<> if q RECURSE then
   v q 10 * - 48 + CHAR+ ;

: SIGNED ( n -- )
   dup 0< if 45 CHAR+ negate then DIGITS ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: TREE ( ptr u8 n n -- )
   {: a:ptr u:n key:n :}
   5 a BYTES>N u key RECORD ;

: HIGHLIGHT ( n -- )
   {: count:n :}
   6 MASK BYTES>N count 0 RECORD ;

: DRAW ( n -- )
   {: count:n :}
   2 TRIANGLES BYTES>N count MATRIX BYTES>N RECORD ;

: U32! ( n ptr u8 -- )
   {: bits:n dst:ptr :}
   4 0 do bits i 8 * rshift $FF and dst i + c! loop ;

: FLOAT! ( n n -- )
   {: bits:n offset:n :}
   bits TRIANGLES offset + U32! ;

: INIT-GEOMETRY ( -- )
   \ Two separate front-facing triangles, one selected.
   $BF666666 0 FLOAT!  $BF333333 4 FLOAT!  $3F000000 8 FLOAT!
   $BDCCCCCD 12 FLOAT! $BF333333 16 FLOAT! $3F000000 20 FLOAT!
   $BF99999A 24 FLOAT! $3F4CCCCD 28 FLOAT! $3F000000 32 FLOAT!
   $3DCCCCCD 36 FLOAT! $BF333333 40 FLOAT! $3F000000 44 FLOAT!
   $3F666666 48 FLOAT! $BF333333 52 FLOAT! $3F000000 56 FLOAT!
   $3F333333 60 FLOAT! $3F4CCCCD 64 FLOAT! $3F000000 68 FLOAT!
   64 0 do 0 MATRIX i + c! loop
   $3F800000 MATRIX U32!
   $3F800000 MATRIX 20 + U32!
   $3F800000 MATRIX 40 + U32!
   $3F800000 MATRIX 60 + U32!
   1 MASK c! 0 MASK 1+ c! 0 MASK 2 + c! ;

: START ( -- )
   INIT-GEOMETRY
   0 EVENT-BUF BYTES>N 0 0 RECORD
   s\" [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"},{\"key\":2,\"parent\":1,\"label\":\"Part\"},{\"key\":3,\"parent\":2,\"label\":\"Face\"}]" 2 TREE
   2 DRAW 2 HIGHLIGHT 2 DRAW ;

: EVENT-TEXT ( -- )
   0 TEXT-U !
   0 EVENT@ SIGNED 58 CHAR+
   1 EVENT@ SIGNED 44 CHAR+ 2 EVENT@ SIGNED
   64 CHAR+ 3 EVENT@ SIGNED 44 CHAR+ 4 EVENT@ SIGNED
   32 CHAR+ 5 EVENT@ SIGNED 44 CHAR+ 6 EVENT@ SIGNED
   0 EVENT@ 6 = if
      s"  pressed " SAY PRESS-BUTTONS @ SIGNED 44 CHAR+ PRESS-MODS @ SIGNED
   then
   TEXT-RECORD ;

: KEY-TEXT ( -- )
   0 TEXT-U !
   s" key " SAY
   1 EVENT@ >BYTES 2 EVENT@ SAY
   32 CHAR+ 3 EVENT@ SIGNED 44 CHAR+ 4 EVENT@ SIGNED
   32 CHAR+ 6 EVENT@ SIGNED
   TEXT-RECORD ;

: IS? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   2 EVENT@ u <> if false exit then
   u 0 ?do 1 EVENT@ i + >BYTES c@ a i + c@ <> if false unloop exit then loop
   true ;

: INVALID ( -- )
   s" json" IS? if s" nope" 0 TREE exit then
   s" utf8" IS? if 255 TEXT c! TEXT 1 0 TREE exit then
   s" duplicate" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\"},{\"key\":1,\"parent\":0,\"label\":\"B\"}]" 0 TREE exit then
   s" parent" IS? if
      s\" [{\"key\":2,\"parent\":1,\"label\":\"Child\"}]" 0 TREE exit then
   s" selected" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"A\"}]" 2 TREE exit then
   s" mask" IS? if 2 MASK c! 2 HIGHLIGHT exit then
   s" short" IS? if 1 HIGHLIGHT 2 DRAW exit then
   s" long" IS? if 3 HIGHLIGHT 2 DRAW exit then
   s" zero" IS? if 0 HIGHLIGHT 0 DRAW exit then
   s" redraw" IS? if 2 DRAW exit then
   s" tree" IS? if
      s\" [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"},{\"key\":2,\"parent\":1,\"label\":\"Part\"},{\"key\":3,\"parent\":2,\"label\":\"Face\"}]" 3 TREE exit then ;

public

: TURN ( -- )
   0 EVENT@ {: kind:n :}
   kind 0 = if START exit then
   kind 4 = if INVALID exit then
   kind 8 = if KEY-TEXT exit then
   kind 2 = if 5 EVENT@ PRESS-BUTTONS ! 6 EVENT@ PRESS-MODS ! then
   EVENT-TEXT ;

;package
